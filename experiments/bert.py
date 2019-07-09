import torch
import torch.utils.data
import torch.nn as nn
import os
from tqdm import tqdm, trange
from mlpipeline.utils import (Versions,
                              ExecutionModeKeys,
                              console_colors,
                              MetricContainer,
                              use_mlflow,
                              version_parameters)
from mlpipeline.helper import (Experiment,
                               DataLoader)
from mlpipeline_torch_utils import (BaseTorchExperiment)
from pytorch_pretrained_bert.modeling import (BertForSequenceClassification,
                                              BertConfig)
from pytorch_pretrained_bert.tokenization import BertTokenizer
from pytorch_pretrained_bert.optimization import BertAdam
from utils import (InputExample,
                   InputFeatures,
                   convert_examples_to_features,
                   DataProcessor)

vp = version_parameters
BERT_MODEL = "bert-base-uncased"
use_cuda = False
device = torch.device("cuda" if torch.cuda.is_available() and not use_cuda else "cpu")
LEARNING_RATE = 5e-5
WARMUP_PROPORTION = 0.1
MAX_SEQ_LENGTH = 32


class BertDataProcessor(DataProcessor):
    def __init__(self, ):
        pass

    def get_train_examples(self):
        return [InputExample("a", "I am not a dog", label="a"),
                InputExample("b", "I am not a cat", label="b")]

    def get_labels(self):
        return ["a", "b"]


class BertDataloader(DataLoader):
    def __init__(self, data_processor, tokenizer, batch_size):
        self.data_processor = data_processor
        self.tokenizer = tokenizer
        self.label_list = self.data_processor.get_labels()
        self.num_labels = len(self.label_list)
        self.batch_size = batch_size

    def get_test_sample_count(self):
        return 1

    def get_train_sample_count(self):
        return len(self.data_processor.get_train_examples())

    def get_test_input(self):
        pass

    def get_train_input(self, mode=ExecutionModeKeys.TRAIN):
        train_features = convert_examples_to_features(
            self.data_processor.get_train_examples(),
            self.data_processor.get_labels(),
            MAX_SEQ_LENGTH,
            self.tokenizer,
            "classification")
        all_input_ids = torch.tensor([f.input_ids for f in train_features], dtype=torch.long)
        all_input_mask = torch.tensor([f.input_mask for f in train_features], dtype=torch.long)
        all_segment_ids = torch.tensor([f.segment_ids for f in train_features], dtype=torch.long)
        all_label_ids = torch.tensor([f.label_id for f in train_features], dtype=torch.long)
        train_data = torch.utils.data.TensorDataset(all_input_ids, all_input_mask,
                                                    all_segment_ids, all_label_ids)
        train_sampler = torch.utils.data.RandomSampler(train_data)
        return torch.utils.data.DataLoader(train_data, sampler=train_sampler, batch_size=self.batch_size)


class BertExperiment(BaseTorchExperiment):
    def pre_execution_hook(self, version, experiment_dir, exec_mode=ExecutionModeKeys.TEST):
        self.current_version = version
        self.dataloader = self.current_version[version_parameters.DATALOADER]
        self.model = BertForSequenceClassification.from_pretrained(
            "experiments/pretrained_models/bert-base-uncased.tar.gz",
            # cache_dir="experiments/pretrained_models",
            num_labels=self.dataloader.num_labels).to(device=device)
        self.log("Loaded {}".format(BERT_MODEL))
        param_optimizer = list(self.model.named_parameters())
        no_decay = ['bias', 'LayerNorm.bias', 'LayerNorm.weight']
        optimizer_grouped_parameters = [
            {'params': [p for n, p in param_optimizer if not any(nd in n for nd in no_decay)],
             'weight_decay': 0.01},
            {'params': [p for n, p in param_optimizer if any(nd in n for nd in no_decay)],
             'weight_decay': 0.0}
        ]
        self.steps = int(
            self.dataloader.get_train_sample_count()/self.current_version[vp.BATCH_SIZE]) * \
            self.current_version[vp.EPOC_COUNT]
        self.optimizer = BertAdam(optimizer_grouped_parameters,
                                  lr=LEARNING_RATE,
                                  warmup=WARMUP_PROPORTION,
                                  t_total=self.steps)

        self.history_file_name = "{}/model_params{}.tch".format(experiment_dir.rstrip("/"), "{}")
        self.file_name = self.history_file_name.format(0)
        if os.path.isfile(self.file_name):
            self.log("Loading parameters from: {}".format(self.file_name))
            self.load_history_checkpoint(self.file_name)

        else:
            self.epocs_params = 0
            self.log("No checkpoint")

    def train_loop(self, input_fn, steps, *args, **kwargs):
        self.model.train()
        global_step = 0
        print(steps, self.current_version[vp.EPOC_COUNT])
        epocs = int(steps/self.current_version[vp.EPOC_COUNT])
        if epocs <= 1:
            epocs = 10
        for _ in trange(epocs, desc="Epoch"):
            tr_loss = 0
            nb_tr_examples, nb_tr_steps = 0, 0
            for step, batch in enumerate(tqdm(input_fn, desc="Iteration")):
                batch = tuple(t.to(device) for t in batch)
                input_ids, input_mask, segment_ids, label_ids = batch

                # define a new function to compute loss values for both output_modes
                logits = self.model(input_ids, segment_ids, input_mask, labels=None)

                loss_fct = nn.CrossEntropyLoss()
                loss = loss_fct(logits.view(-1, self.dataloader.num_labels), label_ids.view(-1))

                loss = loss.mean()  # mean() to average on multi-gpu.
                loss.backward()

                tr_loss += loss.item()
                nb_tr_examples += input_ids.size(0)
                nb_tr_steps += 1
                self.optimizer.step()
                self.optimizer.zero_grad()
                global_step += 1

    def evaluate_loop(self, input_fn, steps):
        return MetricContainer()


dataloader = BertDataloader(data_processor=BertDataProcessor(),
                            tokenizer=BertTokenizer.from_pretrained(BERT_MODEL, do_lower_case=True),
                            batch_size=2)
versions = Versions(dataloader, 2, 10)
versions.add_version("Basic")
EXPERIMENT = BertExperiment(versions)
