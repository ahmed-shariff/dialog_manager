from enum import Enum

class ContextObjectStates(Enum):
    RESOLVED = 1
    UNRESOLVED = 2

class ContextObjectTypes(Enum):
    USER_CONCERN = 1
    USER_RESPONSE = 2
    SYSTEM_CONCERN = 3
    SYSTEM_RESPONSE = 4

class ContextObject():
    def __init__(self, name, id, type, function = None):
        '''
        name: The textual name of the ContextObject
        id: A unique ID for the ContextObject
        type: One of the ContextObjectTypes
        function: A function to be executed when marked resolved. 
                  (applicable only if this has a type ContextObjectTypes.SYSTEM_CONCERN)
        '''
        assert isinstance(type, ContextObjectTypes)
        self.name = name
        self.id = id
        self.type = type
        self.function = function
        self.state = ContextObjectStates.UNRESOLVED
        self.context = []

    def add_context(self, context):
        '''
        Used to store the add a context object to the context of this context object.
        '''
        assert isinstance(context, ContextObject), "Wrong type"
        self.context.append(context)

    def mark_resolved(self):
        self.state = ContextObjectStates.RESOLVED
        # TODO: execute function when resolved

class Context():
    def __init__(self):
        self.root = ContextObject("ROOT", 0, ContextObjectTypes.SYSTEM_CONCERN)
        self.current_id = 1
        self.all_system_concerns_id_map = {"ROOT": 0} # name:id map
        
    def get_active_concerns(self):
        def _get_active_concerns(current_concern, concerns, active_concerns):
            if current_concern.state == ContextObjectStates.UNRESOLVED:
                active_concerns[current_concern.id] = current_concern

            concerns += current_concern.context
            if len(concerns) == 0:
                return active_concerns
            else:
                return _get_active_concerns(concerns[0], concerns, active_concerns)

        return _get_active_concerns(self.root, [], {})

    def _get_id(self):
        return_id = self.current_id
        self.current_id = self.current_id + 1
        return return_id
    
    def add_user_context_object(self, utterance, response_functions, trigger_functions):
        active_concerns = self.get_active_concerns()
        context_object = ContextObject(utterance,
                                       self._get_id(),
                                       ContextObjectTypes.USER_CONCERN if len(trigger_functions) > 0 \
                                       else ContextObjectTypes.USER_RESPONSE)

        for trigger_function in trigger_functions:
            trigger_context_object = self.add_system_context_object(self, trigger_function)
            context_object.add_context(trigger_context_object)
        
        for idx in function_name_list_to_id_list(response_functions):
            active_concerns[idx].add_context(context_object)

        return context_object
        
    def function_name_list_to_id_list(self, name_list):
        return [self.all_system_concerns_id_map[name] for name in name_list]

class Dialogue_Manager():
    def __init__(self, response_function_model, trigger_function_model):
        self.context = Context()
        self.response_function_model = response_function_model
        self.trigger_function_model = trigger_function_model

    def process_user_utterance(self, utterance):
        # Get the predicted response and trigger functions
        response_functions = self._get_response_function(utterance)
        trigger_functions = self._get_trigger_functions(utterance)

        isConcern = len(trigger_functions) > 0
        
    def _get_trigger_functions(self, utterance):
        '''
        This function processes the utterance using the trigger_function_model and does any other 
        filtering that is required. It will return the list of possible trigger functions related
        to the utterance. Returnes an empty list if there are not trigger function.
        '''
        # Models output
        trigger_functions = self.trigger_function_model(utterance)
        
        if len(trigger_functions) == 0:
            return []
        else:
            # For now, the model only returns the top result
            # TODO: extend this to return more than one function when appropriate
            max_prob = max(trigger_functions_reversed.values())
            return [n for n,p in trigger_functions.items() if p == max_prob]

    def _get_response_function(self, utterance):
        '''
        Returns the list of response functions for the utterance. It's is first processed through the response_function_model.
        Will raise DialogueProcessingException if the calculated response functions list is empty.
        '''
        response_functions = self.response_function_model(utterance)
        # Get the active concerns
        active_concerns = self.context.get_active_concerns()

        # Filter the trigger functions based on the active concerns
        response_functions = {n:p for n,p in response_functions.items() if n in active_concerns}

        if len(response_functions) == 0:
            raise DialogueProcessingException("No response functions")
        return [n for n,p in response_functions.items()]
    

class DialogueProcessingException(Exception):
    pass
