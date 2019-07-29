from tqdm import tqdm
from mlpipeline.base import (DataLoaderABC, ExperimentABC, DataLoaderCallableWrapper)
from mlpipeline import Versions, MetricContainer, iterator
from mlpipeline.utils import add_script_dir_to_PATH

from fastai.basic_train import load_learner

from components import (Dialogue_Manager,
                        FunctionResolverBase,
                        Function,
                        ParamtereExtractionFunctionBase)
from data_utils import LoadDatasetDialogue, get_functions_from_utterance
from dialogue_manager_test import TestFunctionResolver, get_function_index

import pandas as pd

add_script_dir_to_PATH("../data")
import templates
from process_dialogue_babi import _functions

pd.set_option("display.max_colwidth", 100)

TRN_DATA_FILE = "../data/generated_dataset_trn.json"
DEV_DATA_FILE = "../data/generated_dataset_dev.json"
TST_DATA_FILE = "../data/generated_dataset_tst.json"
TST_OOV_DATA_FILE = "../data/generated_dataset_tst-OOV.json"

function_groups_dialog_babi = [[_functions.book_table, [_functions.book_table_city,
                                                        _functions.book_table_count,
                                                        _functions.book_table_cuisine,
                                                        _functions.book_table_price]]]
function_groups_generated = templates.function_groups
function_groups_generated += function_groups_dialog_babi


class Model:
    def __init__(self, root_model_path, fwd_model_file_name, bwd_model_file_name, function_groups):
        root_model_path = "outputs/experiment_ckpts/ulmfit-dialog_babi_data_model"
        self.fwd_model = load_learner(root_model_path, 'export_fwd.pkl')
        self.bwd_model = load_learner(root_model_path, 'export_bwd.pkl')
        self.ds = self.fwd_model.data.single_ds.y
        self.trigger_functions = []
        self.response_functions = [templates.functions.root_concern]
        for func in function_groups:
            self.trigger_functions.append(func[0])
            self.response_functions.extend(func[1])

    def _get_prediction(self, utterance):
        fwd_pred = self.fwd_model.predict(utterance)
        bwd_pred = self.bwd_model.predict(utterance)
        pred = (fwd_pred[2] + bwd_pred[2]) / 2
        analyze_pred = self.ds.analyze_pred(pred, 0.1)
        output_pred = pred[analyze_pred > 0].tolist()
        output_class = self.ds.reconstruct(analyze_pred).obj
        return zip(output_class, output_pred)

    def get_trigger_function_model(self):
        def trigger_function_model(utterance):
            predictions = list(self._get_prediction(utterance))
            return {func: pred for func, pred in predictions if func in self.trigger_functions}
        return trigger_function_model

    def get_response_function_model(self):
        def response_function_model(utterance):
            predictions = list(self._get_prediction(utterance))
            return {func: pred for func, pred in predictions if func in self.response_functions}
        return response_function_model


class DataLoader(DataLoaderABC):
    def __init__(self, test_file_path, test_oov_file_path, from_filter=None):
        load_data_fn = LoadDatasetDialogue(from_filter)
        self.test_data = load_data_fn(test_file_path)
        self.test_oov_data = load_data_fn(test_oov_file_path)

    def get_test_input(self):
        return self.test_data, self.test_oov_data


class Experiment(ExperimentABC):
    def setup_model(self):
        self.model = Model(self.current_version['root_model_path'],
                           'export_fwd.pkl',
                           'export_bwd.pkl',
                           self.current_version['function_groups'])

        self.dm = Dialogue_Manager(self.model.get_response_function_model(),
                                   self.model.get_trigger_function_model(),
                                   get_function_index(*self.current_version['functions']),
                                   TestFunctionResolver())
        pass

    def evaluate_loop(self, input_fn, **kwargs):
        metricContainer = MetricContainer(['test_accuracy', 'test_oov_accuracy'])
        test_data, test_oov_data = input_fn
        # print(*test_data, *test_oov_data, sep='\n')
        self.log("Testing on test_data")
        for data in iterator(tqdm(test_data), 5):
            output = self._evaluate_dialogue(*data)
            metricContainer.test_accuracy.update(int(output), 1)
        self.log("Testing on test_data_oov")
        for data in iterator(tqdm(test_oov_data), 5):
            output = self._evaluate_dialogue(*data)
            metricContainer.test_oov_accuracy.update(int(output), 1)
        return metricContainer

    def _evaluate_dialogue(self, dialogue, function_map):
        completed_functions = []
        next_func = templates.functions.root_concern
        while next_func is not None:
            utterance = dialogue.loc[function_map[next_func]]
            functions = get_functions_from_utterance(utterance)
            if len([f for f in functions if f in completed_functions]) != 0:
                return False
            completed_functions.extend(functions)
            self.dm.process_user_utterance(utterance['utterance'])
            next_func = self.dm.get_next_system_concern()
            # print(utterance, functions, next_func, next_func is not None, "\n\n")
        # If there are any functions in the function_map not in the completed_functions
        # the dialogue failed
        if len([f for f in function_map.keys() if f not in completed_functions]) != 0:
            return False
        return True


v = Versions(None, 1, 1)

v.add_version('temp',
              dataloader=DataLoaderCallableWrapper(DataLoader,
                                                   test_file_path=TRN_DATA_FILE,
                                                   test_oov_file_path=DEV_DATA_FILE,
                                                   from_filter='dialog_babi'),
              custom_paramters={
                  'root_model_path': "outputs/experiment_ckpts/ulmfit-generated_data_model",
                  # 'root_model_path': "outputs/experiment_ckpts/ulmfit-generated_data_model",
                  'functions': [templates.order_taxi, templates.book_room,
                                templates.book_ticket, templates.book_table],
                  'function_groups': function_groups_generated
                  })


EXPERIMENT = Experiment(v, False)
