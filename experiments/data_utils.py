import pandas as pd
import itertools
from mlpipeline import iterator
from tqdm import tqdm


def _load_data_to_pd(file_path):
    return pd.read_json(file_path, orient='index')


def function_filter(df, function_filter):
    if function_filter is not None:
        df_functions = df[df['trigger_functions'].apply(lambda x: x is not None and function_filter in x)]
        df = df[df['dialogue_id'].isin(df_functions['dialogue_id'])]
    return df


class LoadDatasetUtterance:
    COMBINED_FUNCTIONS = 1
    TRIGGER_FUNCTIONS = 2
    RESPONSE_FUNCTIONS = 3

    def __init__(self, function_set, function_filter=None):
        self.function_filter = function_filter
        self.function_set = function_set

    def __call__(self, file_path):
        df = _load_data_to_pd(file_path)

        df = df[df['by'] == 'user']
        df = function_filter(df, self.function_filter)
        df = df[~df['response_functions'].isnull() | ~df['trigger_functions'].isnull()]

        if self.function_set == self.COMBINED_FUNCTIONS:
            def get_list(x): return x if x is not None else []
            df.loc[:, "functions"] = pd.Series([list(itertools.chain(get_list(row['trigger_functions']),
                                                                     get_list(row['response_functions'])))
                                                for _, row in df.iterrows()], index=df.index)
            df = df.loc[:, ['utterance', 'functions']]
        elif self.function_set == self.TRIGGER_FUNCTIONS:
            df = df.loc[:, ['utterance', 'trigger_functions']].rename(
                columns={'trigger_functions': 'functions'})
            df['functions'] = df['functions'].apply(lambda x: ['None'] if x is None else x)
        elif self.function_set == self.RESPONSE_FUNCTIONS:
            df = df.loc[:, ['utterance', 'response_functions']].rename(
                columns={'response_functions': 'functions'})
        used_labels = df['functions']
        return df, set(list(itertools.chain(*used_labels[~used_labels.isnull()].to_list())))


class LoadDatasetDialogue:
    def __init__(self, function_filter=None):
        self.function_filter = function_filter

    def __call__(self, file_path):
        self.df = _load_data_to_pd(file_path)
        self.df = function_filter(self.df, self.function_filter)
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
