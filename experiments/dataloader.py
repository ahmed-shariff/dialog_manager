import pandas as pd
import itertools
from mlpipeline.base import DataLoaderABC
from mlpipeline.entities import ExecutionModeKeys


class LoadDataset:
    def __init__(self, from_filter=None):
        self.from_filter = from_filter

    def __call__(self, file_path):
        df = pd.read_json(file_path, orient='index')

        df = df[df['by'] == 'user']
        if self.from_filter is not None:
            df = df[df['dataset'] == self.from_filter]
        df = df[~df['response_functions'].isnull() | ~df['trigger_functions'].isnull()]

        def get_list(x): return x if x is not None else []
        df.loc[:, "functions"] = pd.Series([list(itertools.chain(get_list(row['trigger_functions']),
                                                                 get_list(row['response_functions'])))
                                            for _, row in df.iterrows()], index=df.index)
        df = df.loc[:, ['utterance', 'functions']]
        used_labels = df['functions']
        return df, set(list(itertools.chain(*used_labels.to_list())))


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
