import os
import shutil
from pathlib import Path
from fastai.text import (TextLMDataBunch,
                         TextDataBunch,
                         load_data,
                         load_learner,
                         # TextList,
                         # language_model_learner,
                         accuracy_thresh,
                         text_classifier_learner,
                         AWD_LSTM)
from fastai.text.learner import (_model_meta,
                                 get_language_model,
                                 LanguageLearner)
import fastprogress.fastprogress
from mlpipeline.entities import ExecutionModeKeys
from mlpipeline import (Versions,
                        MetricContainer)
from mlpipeline.utils import Datasets
from mlpipeline.base import ExperimentABC, DataLoaderABC
from data_utils import LoadDatasetUtterance

TRN_DATA_FILE = "../data/generated_dataset_trn.json"
DEV_DATA_FILE = "../data/generated_dataset_dev.json"
TST_DATA_FILE = "../data/generated_dataset_tst.json"
TST_OOV_DATA_FILE = "../data/generated_dataset_tst-OOV.json"

BATCH_SIZE = 16  # 256/16
LR_DIV_FACTOR = 256/BATCH_SIZE
BPTT = 80
PRE_TRAINED_FILES = Path("pretrained_models/")


# Taken from: https://github.com/fastai/fastai/blob/master/fastai/text/learner.py#L201
def language_model_learner(data, arch,
                           config=None,
                           drop_mult=1.,
                           pretrained=True,
                           pretrained_fnames=None, **learn_kwargs):
    "Create a `Learner` with a language model from `data` and `arch`."
    model = get_language_model(arch, len(data.vocab.itos), config=config, drop_mult=drop_mult)
    meta = _model_meta[arch]
    learn = LanguageLearner(data, model, split_func=meta['split_lm'], **learn_kwargs)
    # url = 'url_bwd' if data.backwards else 'url'
    fnames = [PRE_TRAINED_FILES/f'{fn}.{ext}' for fn, ext in zip(pretrained_fnames, ['pth', 'pkl'])]
    learn.load_pretrained(*fnames)
    learn.freeze()
    return learn


class DataLoader(DataLoaderABC):
    def __init__(self, datasets, **kwargs):
        super().__init__(**kwargs)
        self.datasets = datasets

    def get_train_input(self, mode=ExecutionModeKeys.TRAIN, **kargs):
        return self.datasets.train_dataset

    def get_test_input(self, **kargs):
        return self.datasets.test_dataset

    def get_validation_input(self, **kwargs):
        return self.datasets.validation_dataset

    def get_dataloader_summery(self, **kargs):
        return self.summery


class Experiment(ExperimentABC):
    def pre_execution_hook(self, mode=ExecutionModeKeys.TEST):
        self.data_lm_name = "data_lm.pkl"
        self.data_class_name = "data_class_name"
        self.fwd_enc_name = "fwd_enc"
        self.bwd_enc_name = "bwd_enc"
        self.fwd_class_name = 'fwd_clas'
        self.bwd_class_name = 'bwd_clas'

        # to make sure the outputs are also logged
        fastprogress.fastprogress.WRITER_FN = self._get_master_bar_write_fn()

        data_lm_path = os.path.join(self.experiment_dir, self.data_lm_name)
        if not os.path.exists(os.path.dirname(data_lm_path)):
            os.makedirs(os.path.dirname(data_lm_path))
        if not os.path.exists(data_lm_path):
            data_lm = TextLMDataBunch.from_df(path=self.experiment_dir,
                                              train_df=self.dataloader.get_train_input(),
                                              valid_df=self.dataloader.get_test_input(),
                                              text_cols='utterance',
                                              bs=BATCH_SIZE)
            data_lm.save(self.data_lm_name)
        self.data_lm = load_data(self.experiment_dir, self.data_lm_name, bs=BATCH_SIZE, bptt=BPTT)
        self.data_bwd = load_data(self.experiment_dir, self.data_lm_name, bs=BATCH_SIZE,
                                  bptt=BPTT, backwards=True)

        data_class_path = os.path.join(self.experiment_dir, self.data_class_name)
        if not os.path.exists(data_class_path):
            data_class = TextDataBunch.from_df(path=self.experiment_dir,
                                               train_df=self.dataloader.get_train_input(),
                                               valid_df=self.dataloader.get_test_input(),
                                               text_cols='utterance',
                                               label_cols='functions',
                                               vocab=data_lm.train_ds.vocab,
                                               bs=BATCH_SIZE)
            data_class.save(self.data_class_name)
        self.data_class = load_data(self.experiment_dir, self.data_class_name, bs=BATCH_SIZE)
        self.data_class_bwd = load_data(self.experiment_dir, self.data_class_name, bs=BATCH_SIZE, backwards=True)

    def train_loop(self, input_fn, *args, **kwargs):
        # return
        # lm forward
        self._train_encoder(self.data_lm, self.fwd_enc_name, ["wt103-fwd/lstm_fwd", "wt103-fwd/itos_wt103"])
        # lm backward
        self._train_encoder(self.data_bwd, self.bwd_enc_name, ["wt103-bwd/lstm_bwd", "wt103-bwd/itos_wt103"])

        # class forward
        self.class_fwd = self._train_classifier(self.data_class, self.fwd_enc_name, self.fwd_class_name)
        # class backwards
        self.class_bwd = self._train_classifier(self.data_class_bwd, self.bwd_enc_name, self.bwd_class_name)

    def _train_encoder(self, data, encoder_name, pretrained_fnames):
        learn = language_model_learner(data, AWD_LSTM, pretrained_fnames=pretrained_fnames)
        try:
            path = f'{encoder_name}'
            learn.load_encoder(path)
            self.log('Loaded pretrained encoder from {}'.format(str(path)))
        except FileNotFoundError:
            self.log(f'Training encoder `{encoder_name}`')
            learn = learn.to_fp16(clip=0.1)
            learn.fit_one_cycle(1, 2e-2/LR_DIV_FACTOR, moms=(0.8, 0.7), wd=0.1)
            learn.unfreeze()
            learn.fit_one_cycle(10, 2e-3/LR_DIV_FACTOR, moms=(0.8, 0.7), wd=0.1)
            self.log(f"Saving encoder `{encoder_name}`")
            learn.save_encoder(encoder_name)

    def _train_classifier(self, data_class, encoder_name, classifier_name):
        learn = text_classifier_learner(data_class, AWD_LSTM, drop_mult=0.5, pretrained=False)
        try:
            path = f'{classifier_name}'
            learn.load(path)
            self.log('Loaded pretrained classifier from {}'.format(str(path)))
            # return learn
        except FileNotFoundError:
            self.log(f'Training classifier `{classifier_name}`')
            learn.load_encoder(encoder_name)
            lr = 1e-1/LR_DIV_FACTOR
            learn.fit_one_cycle(1, lr, moms=(0.8, 0.7))

            # TODO remove
            # learn.save(classifier_name)
            # return learn
            learn.freeze_to(-2)
            lr /= 2
            learn.fit_one_cycle(1, slice(lr/(2.6**4), lr), moms=(0.8, 0.7))

            learn.freeze_to(-3)
            lr /= 2
            learn.fit_one_cycle(1, slice(lr/(2.6**4), lr), moms=(0.8, 0.7))

            learn.unfreeze()
            lr /= 5
            learn.fit_one_cycle(2, slice(lr/(2.6**4), lr), moms=(0.8, 0.7))
            self.log(f"Saving classifier `{classifier_name}`")
            learn.save(classifier_name)
        return learn

    def post_execution_hook(self, **kwargs):
        metrics = MetricContainer(['validation_accuracy_test', 'validation_accuracy_test_OOV'])
        metrics.validation_accuracy_test.update(self._get_accuracy(self.data_class.valid_dl).item(), 1)
        # print(self._get_accuracy(self.data_class.train_dl,
        #                          self.data_class_bwd.train_dl).item())
        # line = self.dataloader.get_train_input().iloc[0]
        # print(self.class_fwd.predict(line['utterance'])[0].obj)

        dl = self.current_version["oov_df"]()
        oov_db = TextDataBunch.from_df(path=self.experiment_dir,
                                       train_df=dl.get_test_input(),
                                       valid_df=dl.get_test_input(),
                                       text_cols='utterance',
                                       label_cols='functions',
                                       vocab=self.data_lm.train_ds.vocab,
                                       bs=BATCH_SIZE)
        # oov_db_bwd = TextDataBunch.from_df(path=self.experiment_dir,
        #                                    train_df=dl.get_test_input(),
        #                                    valid_df=dl.get_test_input(),
        #                                    text_cols='utterance',
        #                                    label_cols='functions',
        #                                    vocab=self.data_lm.train_ds.vocab,
        #                                    bs=BATCH_SIZE,
        #                                    backwards=True)
        metrics.validation_accuracy_test_OOV.update(self._get_accuracy(oov_db.valid_dl).item(), 1)
        metrics.log_metrics()
        self.class_fwd.export('export_fwd.pkl')
        self.class_bwd.export('export_bwd.pkl')

    def _get_accuracy(self, dl):
        dl_bwd = dl_fwd = dl
        valid_pred_fwd, lbl_fwd = self.class_fwd.get_preds(dl_fwd, ordered=True)
        #print(accuracy_thresh(valid_pred_fwd, lbl_fwd.long(), sigmoid=False))
        valid_pred_bwd, lbl_bwd = self.class_bwd.get_preds(dl_bwd, ordered=True)
        #print(accuracy_thresh(valid_pred_bwd, lbl_fwd.long(), sigmoid=False))

        valid_pred = (valid_pred_bwd + valid_pred_fwd) / 2
        return accuracy_thresh(valid_pred, lbl_fwd.long(), sigmoid=False)

    def _get_master_bar_write_fn(self):
        def write_fn(line, end=None):
            if end is not None:
                print(line, end=end)
            else:
                print(end="\r")
                self.log(line)
        return write_fn

    def clean_experiment_dir(self, model_dir):
        self.log("CLEANING MODEL DIR")
        shutil.rmtree(model_dir, ignore_errors=True)


v = Versions(None, 1, 1)


v.add_version("generated_data_model",
              dataloader=lambda: DataLoader(Datasets(train_dataset_file_path=TRN_DATA_FILE,
                                                     test_dataset_file_path=TST_DATA_FILE,
                                                     train_data_load_function=LoadDatasetUtterance(),
                                                     validation_size=0)),
              custom_paramters={
                  "oov_df": lambda: DataLoader(Datasets(
                      test_dataset_file_path=TST_OOV_DATA_FILE,
                      test_data_load_function=LoadDatasetUtterance())),
              })

v.add_version("dialog_babi_data_model",
              dataloader=lambda: DataLoader(Datasets(train_dataset_file_path=TRN_DATA_FILE,
                                                     test_dataset_file_path=TST_DATA_FILE,
                                                     train_data_load_function=LoadDatasetUtterance('dialog_babi'),
                                                     validation_size=0)),
              custom_paramters={
                  "oov_df": lambda: DataLoader(Datasets(
                      test_dataset_file_path=TST_OOV_DATA_FILE,
                      test_data_load_function=LoadDatasetUtterance('dialog_babi'))),
              })
EXPERIMENT = Experiment(v, True)
