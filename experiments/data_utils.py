import pandas as pd
import itertools
from mlpipeline import iterator
from tqdm import tqdm

def _load_data_to_pd(file_path):
    return pd.read_json(file_path, orient='index')


class LoadDatasetUtterance:
    def __init__(self, from_filter=None):
        self.from_filter = from_filter

    def __call__(self, file_path):
        df = _load_data_to_pd(file_path)

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


class LoadDatasetDialogue:
    def __init__(self, from_filter=None):
        self.from_filter = from_filter

    def __call__(self, file_path):
        self.df = _load_data_to_pd(file_path)
        if self.from_filter is not None:
            self.df = self.df[self.df['dataset'] == self.from_filter]
        dialogue_ids = self.df['dialogue_id'].unique()
        dialogues = []
        for dialogue_id in iterator(tqdm(dialogue_ids, "Loading dialogues: "), 20):
            dialogues.append(self._process_dialogue(dialogue_id))
        return dialogues

    def _process_dialogue(self, dialogue_id):
        function_map = {}
        dialogue_df = self.df[self.df['dialogue_id'] == dialogue_id]
        dialogue_df = dialogue_df[~dialogue_df['reply_to'].isna()]
        dialogue_df = dialogue_df.set_index('turn_id')
        for idx, utterance in dialogue_df[dialogue_df['by'] == 'user'].iterrows():
            functions = get_functions_from_utterance(utterance)
            for function in functions:
                assert function not in function_map
                function_map[function] = idx
        return (dialogue_df, function_map)

def get_functions_from_utterance(utterance):
    functions = utterance[['response_functions', 'trigger_functions']].dropna()
    functions = list(itertools.chain(*functions.to_list()))
    return functions
