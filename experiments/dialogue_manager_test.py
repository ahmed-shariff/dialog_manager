import os
from mlpipeline.utils import add_script_dir_to_PATH
add_script_dir_to_PATH("../data")
import templates
from components import (Dialogue_Manager,
                        FunctionResolverBase,
                        Function,
                        ParamtereExtractionFunctionBase)


TEST_DIALOGUE = [
    # {'dialogue_id': 0, 'by': 'system', 'turn_id': 1, 'reply_to': 0,
    #  'utterance': 'How can I help you',
    #  'response_functions': None,
    #  'trigger_functions': None},
    {'dialogue_id': 0, 'by': 'user', 'turn_id': 2, 'reply_to': 1,
     'utterance': "i'd like to book six rooms in madrid",
     'response_functions': ['book_room_number', 'book_room_city', 'root_concern'],
     'trigger_functions': ['book_room']},
    # {'dialogue_id': 0, 'by': 'system', 'turn_id': 3, 'reply_to': 2,
    #  'utterance': 'How many nights will you be staying for',
    #  'response_functions': None,
    #  'trigger_functions': None},
    {'dialogue_id': 0, 'by': 'user', 'turn_id': 4, 'reply_to': 3,
     'utterance': "i'll be needing them for four nights",
     'response_functions': ['book_room_nights'],
     'trigger_functions': None},
    # {'dialogue_id': 0, 'by': 'system', 'turn_id': 5, 'reply_to': 4,
    #  'utterance': 'In what price range will you be reserving',
    #  'response_functions': None,
    #  'trigger_functions': None},
    {'dialogue_id': 0, 'by': 'user', 'turn_id': 6, 'reply_to': 5,
     'utterance': "i'll take expensive",
     'response_functions': ['book_room_price'],
     'trigger_functions': None}]


def test_response_function_model(utterance):
    for turn in TEST_DIALOGUE:
        # print(utterance in turn['utterance'], {func: 1 for func in turn['response_functions']})
        if utterance in turn['utterance']:
            return {func: 1 for func in turn['response_functions']}
    raise Exception()


def test_trigger_function_model(utterance):
    for turn in TEST_DIALOGUE:
        if utterance in turn['utterance']:
            if turn['trigger_functions']:
                return {func: 1 for func in turn['trigger_functions']}
            else:
                return {}
    raise Exception()


class TestFunctionResolver(FunctionResolverBase):
    def __init__(self):
        pass

    def resolve(self, function, context):
        if function.name == 'root_concern':
            return True, []
        if isinstance(function.function, ParamtereExtractionFunctionBase):
            if len(context) == 0:
                return False, []
            function.function_output = "<placeholder value>"
            return True, []
        else:
            parameters = {
                context_obj.name.replace(function.name + "_", ""): context_obj.function.function_output
                for context_obj in context}
            output = function.function(**parameters)
            return output, []


def get_function_index(*functions):
    function_index = {}
    for function in functions:
        parameters = {}
        function_name = function.__name__
        for parameter in function.__code__.co_varnames:
            param_name = f"{function_name}_{parameter}"
            parameters[param_name] = Function(param_name, ParamtereExtractionFunctionBase(), True)
        function_index[function_name] = Function(function_name, function, False, parameters)
        function_index.update(parameters)
    return function_index


def test_dialogue():
    dm = Dialogue_Manager(test_response_function_model,
                          test_trigger_function_model,
                          get_function_index(templates.order_taxi,
                                             templates.book_room,
                                             templates.book_ticket),
                          TestFunctionResolver())
    print(TEST_DIALOGUE[0]['utterance'])
    dm.process_user_utterance(TEST_DIALOGUE[0]['utterance'])
    finished = TEST_DIALOGUE[0]['response_functions']
    rest = {diag['response_functions'][0]: diag for diag in TEST_DIALOGUE[1:]}
    line = dm.get_next_system_concern()
    if line in finished:
        raise
    finished.append(line)
    print(line, rest[line]['utterance'])
    dm.process_user_utterance(rest[line]['utterance'])

    line = dm.get_next_system_concern()
    if line in finished:
        raise
    finished.append(line)
    print(line, rest[line]['utterance'])
    dm.process_user_utterance(rest[line]['utterance'])
    dm.get_next_system_concern()


if __name__ == '__main__':
    add_script_dir_to_PATH(os.path.abspath("../"))
    test_dialogue()
