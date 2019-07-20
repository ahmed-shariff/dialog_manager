import torch
import itertools
import torch.utils.data
import torch.nn as nn
import os
import json
import pandas as pd
from tqdm import tqdm, trange
from fastai.text import (TextLMDataBunch,
                         TextDataBunch,
                         load_data,
                         language_model_learner,
                         text_classifier_learner,
                         AWD_LSTM)
from sklearn.model_selection import train_test_split
from mlpipeline.base import DataLoaderABC
from mlpipeline.utils import (ExecutionModeKeys,
                              Versions,
                              MetricContainer,
                              version_parameters)
from mlpipeline_torch_utils import (BaseTorchExperiment)
DATA_FILE = "ParlAI/data/dialog-bAbI/dialog-bAbI-tasks/dialog-babi-task1-API-calls-dev.json"
BATCH_SIZE = 16  # 256/16
BPTT = 80


class DataLoader(DataLoaderABC):
    def __init__(self, **kwargs):
        df = pd.read_json(DATA_FILE)
        df = df[df['by'] == 'user']
        df = df[~df['response_functions'].isnull() | ~df['trigger_functions'].isnull()]

        def get_list(x): return x if x is not None else []
        df.loc[:, "functions"] = pd.Series([list(itertools.chain(get_list(row['trigger_functions']),
                                                                 get_list(row['response_functions'])))
                                            for _, row in df.iterrows()], index=df.index)
        df = df.loc[:, ['utterance', 'functions']]
        self.df_train, self.df_valid = train_test_split(df, stratify=df['functions'], random_state=100, test_size=0.25)
        self._train_data_size = len(self.df_train)
        self._test_data_size = len(self.df_valid)
        self.log("Train dataset size: {}".format(self._train_data_size))
        self.log("Validation dataset size: {}".format(self._test_data_size))

    def get_train_input(self, mode=ExecutionModeKeys.TRAIN, **kargs):
        return self.df_train

    def get_test_input(self, **kargs):
        return self.df_valid

    def get_dataloader_summery(self, **kargs):
        return self.summery

    def get_train_sample_count(self):
        return self._train_data_size

    def get_test_sample_count(self):
        return self._test_data_size


class Experiment(BaseTorchExperiment):
    def setup_model(self, version, experiment_dir):
        super().setup_model(version, experiment_dir)

    def pre_execution_hook(self, version, experiment_dir, exec_mode=ExecutionModeKeys.TEST):
        super().pre_execution_hook(version, experiment_dir, exec_mode)
        self.data_lm_name = "data_lm.pkl"
        self.data_class_name = "data_class_name"
        self.fwd_enc_name = "fwd_enc"
        self.bwd_enc_name = "bwd_enc"
        self.fwd_class_name = 'fwd_clas'
        self.bwd_class_name = 'bwd_clas'

        dataloader = version[version_parameters.DATALOADER]()
        data_lm_path = os.path.join(experiment_dir, self.data_lm_name)
        if not os.path.exists(os.path.dirname(data_lm_path)):
            os.makedirs(os.path.dirname(data_lm_path))
        if not os.path.exists(data_lm_path):
            data_lm = TextLMDataBunch.from_df(path=experiment_dir,
                                              train_df=dataloader.get_train_input(),
                                              valid_df=dataloader.get_test_input(),
                                              bs=BATCH_SIZE)
            data_lm.save(self.data_lm_name)
        self.data_lm = load_data(experiment_dir, self.data_lm_name, bs=BATCH_SIZE, bptt=BPTT)
        self.data_bwd = load_data(experiment_dir, self.data_lm_name, bs=BATCH_SIZE, bptt=BPTT, backwards=True)

        data_class_path = os.path.join(experiment_dir, self.data_class_name)
        if not os.path.exists(data_class_path):
            data_class = TextDataBunch.from_df(path=experiment_dir,
                                               train_df=dataloader.get_train_input(),
                                               valid_df=dataloader.get_test_input(),
                                               text_cols='utterance',
                                               label_cols='functions',
                                               vocab=data_lm.train_ds.vocab,
                                               bs=BATCH_SIZE)
            data_class.save()
        self.data_class = load_data(experiment_dir, self.data_class_name, bs=BATCH_SIZE)
        self.data_class_bwd = load_data(experiment_dir, self.data_class_name, bs=BATCH_SIZE, backwards=True)

    def train_loop(self, input_fn, steps, version, *args, **kwargs):
        # lm forward
        self._train_encoder(self.data_lm, self.fwd_enc_name)
        # lm backward
        self._train_encoder(self.data_bwd, self.bwd_enc_name)

        # class forward
        self._train_classifier(self.data_class, self.fwd_enc_name, self.bwd_class_name)
        # class backwards
        self._train_classifier(self.data_class_bwd, self.bwd_enc_name, self.bwd_class_name)

    def _train_encoder(self, data, encoder_name):
        learn = language_model_learner(data, AWD_LSTM)
        learn = learn.to_fp16(clip=0.1)
        learn.fit_one_cycle(1, 2e-2, moms=(0.8, 0.7), wd=0.1)
        learn.unfreeze()
        learn.fit_one_cycle(10, 2e-3, moms=(0.8, 0.7), wd=0.1)
        learn.save_encoder(encoder_name)

    def _train_classifier(self, data_class, encoder_name, classifier_name):
        learn = text_classifier_learner(data_class, AWD_LSTM, drop_mult=0.5, pretrained=False)
        learn.load_encoder(encoder_name)
        lr = 1e-1
        learn.fit_one_cycle(1, lr, moms=(0.8, 0.7))

        learn.freeze_to(-2)
        lr /= 2
        learn.fit_one_cycle(1, slice(lr/(2.6**4), lr), moms=(0.8, 0.7))

        learn.freeze_to(-3)
        lr /= 2
        learn.fit_one_cycle(1, slice(lr/(2.6**4), lr), moms=(0.8, 0.7))

        learn.unfreeze()
        lr /= 5
        learn.fit_one_cycle(2, slice(lr/(2.6**4), lr), moms=(0.8, 0.7))

        learn.save(classifier_name)

    def evaluate_loop(self, input_fn, steps, *args, **kwargs):
        return MetricContainer()


v = Versions(lambda: DataLoader(), 1, 1)

v.add_version("temp")
EXPERIMENT = Experiment(v)

#train_model()
