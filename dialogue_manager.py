import data.templates
from components import (Dialogue_Manager,
                        ContextObjectStates,
                        ContextObjectTypes,
                        FunctionResolverBase,
                        Function,
                        ParamtereExtractionFunctionBase)

response_function_model = lambda utterance: {}
trigger_function_model = lambda utterance: {}


class FunctionResolver(FunctionResolverBase):
    def __init__(self):
        pass

    def resolve(self, function, context):
        if isinstance(function, ParamtereExtractionFunctionBase):
            function.function_output = "<placeholder value>"
            return True, []
        else:
            parameters = {
                context_obj.name.replace(function.name + "_", ""): context_obj.function.function_output
                for context_obj in context}
            output = function.function(**parameters)
            return True, output


def get_function_index(*functions):
    function_index = {}
    for function in functions:
        parameters = {}
        function_name = function.__name__
        for parameter in function.__code__.co_varname:
            parameters[parameter] = Function(parameter, ParamtereExtractionFunctionBase(), True)
        function_index[function_name] = Function(function_name, function, False, parameters)
        function_index.update(parameters)
    return function_index
