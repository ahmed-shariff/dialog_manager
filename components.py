from enum import Enum


class ContextObjectStates(Enum):
    RESOLVED = 1
    UNRESOLVED = 2


class ContextObjectTypes(Enum):
    USER_CONCERN = 1
    USER_RESPONSE = 2
    SYSTEM_CONCERN = 3
    SYSTEM_RESPONSE = 4


class FunctionResolverBase():
    def resolve(self, function, context):
        '''
        should take two parameters: function (A Function object), and the context (a list of ContextObjects)
        Ideally will execute the function.
        The return values are: exit_code (if the execution was a sucess), the output of the function.        
        '''
        raise NotImplemented


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
        if type in (ContextObjectTypes.SYSTEM_RESPONSE, ContextObjectTypes.USER_CONCERN):
            self.state = ContextObjectStates.RESOLVED
        else:
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


class Function():
    def __init__(self, function_name, function, parameters=None):
        self.name = function_name
        self.function = function
        if parameters is not None:
            self.parameters = {name: Function(name, func) for name, func in parameters.items()}
        else:
            self.parameters = None


class Context():
    def __init__(self, function_index, function_resolver):
        self.root = ContextObject("ROOT", 0, ContextObjectTypes.SYSTEM_CONCERN)
        self.current_id = 1
        self.all_system_concerns_id_map = {"ROOT": 0}  # name:id map
        self.function_index = function_index
        self.function_resolver = function_resolver

    def _get_id(self):
        return_id = self.current_id
        self.current_id = self.current_id + 1
        return return_id

    def _check_children(self, context_obj):
        '''
        Return True if all children have been marked as resolved.
        '''
        for child_context_obj in context_obj.context:
            if child_context_obj.state == ContextObjectStates.UNRESOLVED:
                return False
        return True
        
    def get_active_concerns(self):
        '''
        Traverses throught the tree and list all the unresolved concerns.
        '''
        def _get_active_concerns(current_concern, concerns, active_concerns):
            if current_concern.state == ContextObjectStates.UNRESOLVED:
                active_concerns[current_concern.id] = current_concern

            concerns += current_concern.context
            if len(concerns) == 0:
                return active_concerns
            else:
                return _get_active_concerns(concerns[0], concerns, active_concerns)

        return _get_active_concerns(self.root, [], {})

    def add_user_context_object(self, utterance, response_functions, trigger_functions):
        active_concerns = self.get_active_concerns()
        context_object = ContextObject(utterance,
                                       self._get_id(),
                                       ContextObjectTypes.USER_CONCERN if len(trigger_functions) > 0 \
                                       else ContextObjectTypes.USER_RESPONSE)

        for trigger_function in trigger_functions:
            trigger_context_object = self.add_system_concern_context_object(self, trigger_function)
            context_object.add_context(trigger_context_object)
        
        for idx in function_name_list_to_id_list(response_functions):
            active_concerns[idx].add_context(context_object)

        return context_object

    def create_system_concern_context_object(self, function_name):
        function = self.function_index[function_name]
        context_object = ContextObject(function_name,
                                       self._get_id(),
                                       ContextObjectTypes.SYSTEM_CONCERN,
                                       function)
        try:
            for name, func in function.parameters.items():
                context_object.add_context(ContextObject(name,
                                                         self._get_id(),
                                                         ContextObjectTypes.SYSTEM_CONCERN,
                                                         func))
        except:
            pass
        return context_object

    def process_context(self):
        # returns true if all the child concerns and itself is resolved
        # check if all child concerns are resolved.
        # TODO
        def _process_context(context_objs):
            if len(context_objs) == 0:
                return True
            current_context_obj = context_objs[0]
            context_objs = context_objs[1:]
            if current_context_obj.state == ContextObjectStates.RESOLVED:
                return True
            else:
                # only concerns can be marked as UNRESOLVED, so I can assume it's a concern that comes here.
                _process_context(current_context_obj.context)
                self.resolve_concern(current_context_obj)
                _process_context(context_objs)
        _process_context([self.root])
    
    def resolve_concern(self, context_obj):
        '''
        Return True if concern is/was succesflully resolved.
        '''
        if context_obj.state == ContextObjectStates.RESOLVED:
            return True
        if self._check_children(context_obj):
            if context_obj.function is not None:
                # the function resolver to basically outsource how the function is resolved.
                exec_success, outputs = self.function_resolver.resolve(context_obj.function, context_obj.context)
                if not exec_success:
                    # Rethink this. If the context does not have the expected results in the resolved context, what happens!
                    #return False
                    raise NotImplementedError("")
                else:
                    for output in outputs:
                        context_obj.add_context(ContextObject(output,
                                                              self._get_id(),
                                                              ContextObjectTypes.SYSTEM_RESPONSE,
                                                              None))

            context_obj.state = ContextObjectStates.RESOLVED
            return True
        else:
            return False

    def function_name_list_to_id_list(self, name_list):
        return [self.all_system_concerns_id_map[name] for name in name_list]


class Dialogue_Manager():
    def __init__(self, response_function_model, trigger_function_model, function_index):
        self.context = Context()
        self.response_function_model = response_function_model
        self.trigger_function_model = trigger_function_model
        self.function_index = function_index

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
            return [n for n, p in trigger_functions.items() if p == max_prob]

    def _get_response_function(self, utterance):
        '''
        Returns the list of response functions for the utterance. It's is first processed through the response_function_model.
        Will raise DialogueProcessingException if the calculated response functions list is empty.
        '''
        response_functions = self.response_function_model(utterance)
        # Get the active concerns
        active_concerns = self.context.get_active_concerns()

        # Filter the trigger functions based on the active concerns
        response_functions = {n: p for n, p in response_functions.items() if n in active_concerns}

        if len(response_functions) == 0:
            raise DialogueProcessingException("No response functions")
        return [n for n, p in response_functions.items()]


class DialogueProcessingException(Exception):
    pass
