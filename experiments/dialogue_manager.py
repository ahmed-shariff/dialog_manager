from tqdm import tqdm
from mlpipeline.base import (DataLoaderABC, ExperimentABC, DataLoaderCallableWrapper)
from mlpipeline import Versions, MetricContainer, iterator
from mlpipeline.utils import add_script_dir_to_PATH

from fastai.basic_train import load_learner

from components import (Dialogue_Manager,
                        FunctionResolverBase,
                        Function,
                        ParamtereExtractionFunctionBase,
                        DialogueProcessingException)
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
function_groups = templates.function_groups
function_groups += function_groups_dialog_babi


class ModelCombined:
    def __init__(self, root_model_path, fwd_model_file_name, bwd_model_file_name, threshold=0.5):
        # root_model_path = "outputs/experiment_ckpts/ulmfit-dialog_babi_data_model"
        self.fwd_model = load_learner(root_model_path, fwd_model_file_name)
        self.bwd_model = load_learner(root_model_path, bwd_model_file_name)
        self.ds = self.fwd_model.data.single_ds.y
        self.trigger_functions = []
        self.response_functions = [templates.functions.root_concern]
        self.threshold = threshold
        for func in function_groups:
            self.trigger_functions.append(func[0])
            self.response_functions.extend(func[1])

    def get_prediction(self, utterance):
        fwd_pred = self.fwd_model.predict(utterance)
        bwd_pred = self.bwd_model.predict(utterance)
        pred = (fwd_pred[2] + bwd_pred[2]) / 2
        analyze_pred = self.ds.analyze_pred(pred, self.threshold)
        # print("/n/n--------------------------------")
        # print(self.ds.reconstruct(self.ds.analyze_pred(pred, 0.1)).obj)
        # print(pred[analyze_pred > 0].tolist())
        output_pred = pred[analyze_pred > 0].tolist()
        output_class = self.ds.reconstruct(analyze_pred).obj
        return zip(output_class, output_pred)

    def get_trigger_function_model(self):
        def trigger_function_model(utterance):
            predictions = list(self.get_prediction(utterance))
            return {func: pred for func, pred in predictions if func in self.trigger_functions}
        return trigger_function_model

    def get_response_function_model(self):
        def response_function_model(utterance):
            predictions = list(self.get_prediction(utterance))
            return {func: pred for func, pred in predictions if func in self.response_functions}
        return response_function_model


class ModelSeparated:
    def __init__(self, root_model_path, fwd_model_file_name, bwd_model_file_name, threshold=0.5):
        self.fwd_model_response = load_learner(root_model_path[0], fwd_model_file_name)
        self.bwd_model_response = load_learner(root_model_path[0], bwd_model_file_name)
        self.fwd_model_trigger = load_learner(root_model_path[1], fwd_model_file_name)
        self.bwd_model_trigger = load_learner(root_model_path[1], bwd_model_file_name)
        self.ds_response = self.fwd_model_response.data.single_ds.y
        self.ds_trigger = self.fwd_model_trigger.data.single_ds.y
        self.trigger_functions = []
        self.response_functions = [templates.functions.root_concern]
        self.threshold = threshold
        for func in function_groups:
            self.trigger_functions.append(func[0])
            self.response_functions.extend(func[1])

    def get_prediction(self, utterance, response=True, all=True):
        if all:
            fwd_pred = self.fwd_model_response.predict(utterance)
            bwd_pred = self.bwd_model_response.predict(utterance)
            pred = (fwd_pred[2] + bwd_pred[2]) / 2
            analyze_pred = self.ds_response.analyze_pred(pred, self.threshold)
            output_pred = pred[analyze_pred > 0].tolist()
            output_class = self.ds_response.reconstruct(analyze_pred).obj

            fwd_pred = self.fwd_model_trigger.predict(utterance)
            bwd_pred = self.bwd_model_trigger.predict(utterance)
            pred = (fwd_pred[2] + bwd_pred[2]) / 2
            analyze_pred = self.ds_trigger.analyze_pred(pred, self.threshold)
            output_pred += pred[analyze_pred > 0].tolist()
            output_class += self.ds_trigger.reconstruct(analyze_pred).obj
        else:
            if response:
                fwd_pred = self.fwd_model_response.predict(utterance)
                bwd_pred = self.bwd_model_response.predict(utterance)
                ds = self.ds_response
            else:
                fwd_pred = self.fwd_model_trigger.predict(utterance)
                bwd_pred = self.bwd_model_trigger.predict(utterance)
                ds = self.ds_trigger
            pred = (fwd_pred[2] + bwd_pred[2]) / 2
            analyze_pred = ds.analyze_pred(pred, self.threshold)
            output_pred = pred[analyze_pred > 0].tolist()
            output_class = ds.reconstruct(analyze_pred).obj
        return zip(output_class, output_pred)

    def get_trigger_function_model(self):
        def trigger_function_model(utterance):
            predictions = list(self.get_prediction(utterance, response=False, all=False))
            return {func: pred for func, pred in predictions if func in self.trigger_functions}
        return trigger_function_model

    def get_response_function_model(self):
        def response_function_model(utterance):
            predictions = list(self.get_prediction(utterance, response=True, all=False))
            return {func: pred for func, pred in predictions if func in self.response_functions}
        return response_function_model


class DataLoader(DataLoaderABC):
    def __init__(self, test_file_path, test_oov_file_path, function_filter=None):
        load_data_fn = LoadDatasetDialogue(function_filter)
        self.test_data = load_data_fn(test_file_path)
        self.log(f"Loaded: {test_file_path}")
        self.test_oov_data = load_data_fn(test_oov_file_path)
        self.log(f"Loaded: {test_oov_file_path}")

    def get_test_input(self):
        return self.test_data, self.test_oov_data


class Experiment(ExperimentABC):
    def setup_model(self):
        self.model = self.current_version['model'](self.current_version['root_model_path'],
                                                   'export_fwd.pkl',
                                                   'export_bwd.pkl')

        self.dm = Dialogue_Manager(self.model.get_response_function_model(),
                                   self.model.get_trigger_function_model(),
                                   get_function_index(*self.current_version['functions']),
                                   TestFunctionResolver())
        pass

    def evaluate_loop(self, input_fn, **kwargs):
        metricContainer = MetricContainer(['test_accuracy', 'test_oov_accuracy',
                                           "test_utt_accuracy", "test_oov_utt_accuracy"])
        test_data, test_oov_data = input_fn
        # print(*test_data, *test_oov_data, sep='\n')
        self.log("Testing on test_data")
        for data in iterator(tqdm(test_data), 50):
            output = self._evaluate_dialogue(data)
            for _, utterance in data[0].iterrows():
                if utterance['by'] != 'user':
                    continue
                pred = list(self.model.get_prediction(utterance['utterance']))
                pred = [p for p in pred if 'None' not in p]
                functions = get_functions_from_utterance(utterance)
                metricContainer.test_utt_accuracy.update(
                    1 if len(pred) == len(functions) and len(functions) == len(
                        [k for k, v in pred if k in functions]) else 0,
                    1)
            metricContainer.test_accuracy.update(int(output), 1)
        self.log("Testing on test_data_oov")
        for data in iterator(tqdm(test_oov_data), 50):
            output = self._evaluate_dialogue(data)
            for _, utterance in data[0].iterrows():
                if utterance['by'] != 'user':
                    continue
                pred = list(self.model.get_prediction(utterance['utterance']))
                pred = [p for p in pred if 'None' not in p]
                functions = get_functions_from_utterance(utterance)
                metricContainer.test_oov_utt_accuracy.update(
                    1 if len(pred) == len(functions) and len(functions) == len(
                        [k for k, v in pred if k in functions]) else 0,
                    1)
            metricContainer.test_oov_accuracy.update(int(output), 1)
        return metricContainer

    def _evaluate_dialogue(self, data):
        dialogue, function_map = data
        self.dm.reset_context()
        # print("*"*30)
        self.dm.get_next_system_concern()
        completed_functions = []
        next_func = templates.functions.root_concern
        outed = []
        asked = []
        while next_func is not None:
            asked.append(next_func)
            try:
                if next_func not in function_map:
                    # print("boooooooooooooooooooooooooooooooooooooooooooo")
                    # raise
                    return False
                utterance = dialogue.loc[function_map[next_func]]
                outed.append(utterance['utterance'])
                functions = get_functions_from_utterance(utterance)
                if len([f for f in functions if f in completed_functions]) != 0:
                    # print(completed_functions, functions)
                    # print(outed, sep="\n")
                    # print(asked)
                    # print(1, *self.dm.context.contexts, sep="\n")
                    # print("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
                    # raise
                    return False
                completed_functions.extend(functions)
                try:
                    self.dm.process_user_utterance(utterance['utterance'])
                except DialogueProcessingException:
                    # print(utterance['utterance'], functions)
                    return False
                next_func = self.dm.get_next_system_concern()
            except Exception:
                # print(next_func, dialogue, function_map, asked, outed, completed_functions, self.dm.context.ac, sep="\n")
                raise
        # If there are any functions in the function_map not in the completed_functions
        # the dialogue failed
        if len([f for f in function_map.keys() if f not in completed_functions]) != 0:
            # print(2, *self.dm.context.contexts, sep="\n")
            # print("bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
            # print(next_func, dialogue, function_map, asked, outed, completed_functions, self.dm.context.ac, sep="\n")
            # raise
            return False
        return True


v = Versions(None, 1, 1)
templates.print_function_output = False

v.add_version('complete_combined',
              order=1,
              dataloader=DataLoaderCallableWrapper(DataLoader,
                                                   test_file_path=TST_DATA_FILE,
                                                   test_oov_file_path=TST_OOV_DATA_FILE,
                                                   function_filter=None),
              custom_paramters={
                  'root_model_path': "outputs/experiment_ckpts/ulmfit-complete_combined_data_model",
                  'model': ModelCombined,
                  # 'root_model_path': "outputs/experiment_ckpts/ulmfit-generated_data_model",
                  'functions': [templates.order_taxi, templates.book_room,
                                templates.book_ticket, templates.book_table]})

v.add_version('book_table_combined',
              order=1,
              dataloader=DataLoaderCallableWrapper(DataLoader,
                                                   test_file_path=TST_DATA_FILE,
                                                   test_oov_file_path=TST_OOV_DATA_FILE,
                                                   function_filter='book_table'),
              custom_paramters={
                  'root_model_path': "outputs/experiment_ckpts/ulmfit-book_table_combined_data_model",
                  'model': ModelCombined,
                  # 'root_model_path': "outputs/experiment_ckpts/ulmfit-generated_data_model",
                  'functions': [templates.book_table]})

v.add_version('book_room_combined',
              order=1,
              dataloader=DataLoaderCallableWrapper(DataLoader,
                                                   test_file_path=TST_DATA_FILE,
                                                   test_oov_file_path=TST_OOV_DATA_FILE,
                                                   function_filter='book_room'),
              custom_paramters={
                  'root_model_path': "outputs/experiment_ckpts/ulmfit-book_room_combined_data_model",
                  'model': ModelCombined,
                  'functions': [templates.book_room]})

v.add_version('order_taxi_combined',
              order=1,
              dataloader=DataLoaderCallableWrapper(DataLoader,
                                                   test_file_path=TST_DATA_FILE,
                                                   test_oov_file_path=TST_OOV_DATA_FILE,
                                                   function_filter="order_taxi"),
              custom_paramters={
                  'root_model_path': "outputs/experiment_ckpts/ulmfit-order_taxi_combined_data_model",
                  'model': ModelCombined,
                  # 'root_model_path': "outputs/experiment_ckpts/ulmfit-generated_data_model",
                  'functions': [templates.order_taxi]})


v.add_version('book_ticket_combined',
              order=1,
              dataloader=DataLoaderCallableWrapper(DataLoader,
                                                   test_file_path=TST_DATA_FILE,
                                                   test_oov_file_path=TST_OOV_DATA_FILE,
                                                   function_filter="book_ticket"),
              custom_paramters={
                  'root_model_path': "outputs/experiment_ckpts/ulmfit-book_ticket_combined_data_model",
                  'model': ModelCombined,
                  # 'root_model_path': "outputs/experiment_ckpts/ulmfit-generated_data_model",
                  'functions': [templates.book_ticket]})

# Seperated models
v.add_version('complete_separated',
              order=1,
              dataloader=DataLoaderCallableWrapper(DataLoader,
                                                   test_file_path=TST_DATA_FILE,
                                                   test_oov_file_path=TST_OOV_DATA_FILE,
                                                   function_filter=None),
              custom_paramters={
                  'root_model_path': ["outputs/experiment_ckpts/ulmfit-complete_response_data_model",
                                      "outputs/experiment_ckpts/ulmfit-complete_trigger_data_model"],
                  'model': ModelSeparated,
                  'functions': [templates.order_taxi, templates.book_room,
                                templates.book_ticket, templates.book_table]})

v.add_version('book_table_separated',
              order=1,
              dataloader=DataLoaderCallableWrapper(DataLoader,
                                                   test_file_path=TST_DATA_FILE,
                                                   test_oov_file_path=TST_OOV_DATA_FILE,
                                                   function_filter='book_table'),
              custom_paramters={
                  'root_model_path': ["outputs/experiment_ckpts/ulmfit-book_table_response_data_model",
                                      "outputs/experiment_ckpts/ulmfit-book_table_trigger_data_model"],
                  'model': ModelSeparated,
                  'functions': [templates.book_table]})

v.add_version('book_room_separated',
              order=1,
              dataloader=DataLoaderCallableWrapper(DataLoader,
                                                   test_file_path=TST_DATA_FILE,
                                                   test_oov_file_path=TST_OOV_DATA_FILE,
                                                   function_filter='book_room'),
              custom_paramters={
                  'root_model_path': ["outputs/experiment_ckpts/ulmfit-book_room_response_data_model",
                                      "outputs/experiment_ckpts/ulmfit-book_room_trigger_data_model"],
                  'model': ModelSeparated,
                  'functions': [templates.book_room]})

v.add_version('order_taxi_separated',
              order=1,
              dataloader=DataLoaderCallableWrapper(DataLoader,
                                                   test_file_path=TST_DATA_FILE,
                                                   test_oov_file_path=TST_OOV_DATA_FILE,
                                                   function_filter='order_taxi'),
              custom_paramters={
                  'root_model_path': ["outputs/experiment_ckpts/ulmfit-order_taxi_response_data_model",
                                      "outputs/experiment_ckpts/ulmfit-order_taxi_trigger_data_model"],
                  'model': ModelSeparated,
                  'functions': [templates.order_taxi]})

v.add_version('book_ticket_separated',
              order=1,
              dataloader=DataLoaderCallableWrapper(DataLoader,
                                                   test_file_path=TST_DATA_FILE,
                                                   test_oov_file_path=TST_OOV_DATA_FILE,
                                                   function_filter='book_ticket'),
              custom_paramters={
                  'root_model_path': ["outputs/experiment_ckpts/ulmfit-book_ticket_response_data_model",
                                      "outputs/experiment_ckpts/ulmfit-book_ticket_trigger_data_model"],
                  'model': ModelSeparated,
                  'functions': [templates.book_ticket]})

# outputs/experiment_ckpts/ulmfit-book_ticket_trigger_data_model
# outputs/experiment_ckpts/ulmfit-order_taxi_trigger_data_model
# outputs/experiment_ckpts/ulmfit-book_room_trigger_data_model
# outputs/experiment_ckpts/ulmfit-book_table_trigger_data_model
# outputs/experiment_ckpts/ulmfit-complete_trigger_data_model
# outputs/experiment_ckpts/ulmfit-book_ticket_response_data_model
# outputs/experiment_ckpts/ulmfit-order_taxi_response_data_model
# outputs/experiment_ckpts/ulmfit-book_room_response_data_model
# outputs/experiment_ckpts/ulmfit-book_table_response_data_model
# outputs/experiment_ckpts/ulmfit-complete_response_data_model
# outputs/experiment_ckpts/ulmfit-book_ticket_combined_data_model
# outputs/experiment_ckpts/ulmfit-order_taxi_combined_data_model
# outputs/experiment_ckpts/ulmfit-book_room_combined_data_model
# outputs/experiment_ckpts/ulmfit-book_table_combined_data_model
# outputs/experiment_ckpts/ulmfit-complete_combined_data_model
# v.filter_versions(whitelist_versions=['book_table_separated'])
EXPERIMENT = Experiment(v, False)
