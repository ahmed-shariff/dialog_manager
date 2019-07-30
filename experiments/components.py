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
        raise NotImplementedError()


class Function():
    def __init__(self, function_name, function, user_facing_function, parameters=None):
        self.name = function_name
        self.function = function
        if parameters is None:
            self.parameters = {}
        self.parameters = parameters
        self.function_output = None
        self.user_facing_function = user_facing_function


class ParamtereExtractionFunctionBase():
    def __call__(self, value):
        raise NotImplementedError()


class ContextObject():
    def __init__(self, name, id, type, function=None):
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
        if type in (ContextObjectTypes.SYSTEM_RESPONSE, ContextObjectTypes.USER_RESPONSE):
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


class Context():
    def __init__(self, function_index, function_resolver):
        self._function_index = function_index
        self._function_resolver = function_resolver
        self.reset_context()

    def reset_context(self):
        self._all_system_concerns_id_map = {}  # name:id map
        self._root = self._create_context_object("root_concern",
                                                 0,
                                                 ContextObjectTypes.SYSTEM_CONCERN,
                                                 Function("root_concern", lambda: None, True))
        self._current_id = 1

    def get_active_concerns(self, additional_context=None):
        '''
        Traverses throught the tree and list all the unresolved concerns.
        '''
        def _get_active_concerns(current_concern, concerns, active_concerns, visited_ids):
            if current_concern.id not in visited_ids:
                try:
                    if current_concern.function.user_facing_function and \
                       current_concern.state == ContextObjectStates.UNRESOLVED:
                        active_concerns[current_concern.id] = current_concern
                except AttributeError:
                    pass
                concerns += current_concern.context
                visited_ids.append(current_concern.id)
            if len(concerns) == 0:
                return active_concerns
            return _get_active_concerns(concerns[0], concerns[1:], active_concerns, visited_ids)
        if additional_context is None:
            additional_context = []
        return _get_active_concerns(self._root, additional_context, {}, [])

    def add_user_context_object(self, utterance, response_functions, trigger_functions):
        context_object = self._create_context_object(
            utterance,
            self._get_id(),
            ContextObjectTypes.USER_CONCERN if len(trigger_functions) > 0
            else ContextObjectTypes.USER_RESPONSE)

        additional_context = []
        for trigger_function in trigger_functions:
            trigger_context_object = self._add_system_concern_context_object(trigger_function)
            context_object.add_context(trigger_context_object)
            additional_context.extend(trigger_context_object.context)

        active_concerns = self.get_active_concerns(additional_context)
        for active_concern in active_concerns.values():
            if active_concern.name in response_functions:
                active_concern.add_context(context_object)

        return context_object

    def _create_context_object(self, name, id, type, function=None):
        if function is not None:
            self._all_system_concerns_id_map[name] = id
        return ContextObject(name, id, type, function)

    def _add_system_concern_context_object(self, function_name):
        function = self._function_index[function_name]
        context_object = self._create_context_object(function_name,
                                                     self._get_id(),
                                                     ContextObjectTypes.SYSTEM_CONCERN,
                                                     function)
        try:
            for name, func in function.parameters.items():
                context_object.add_context(self._create_context_object(name,
                                                                       self._get_id(),
                                                                       ContextObjectTypes.SYSTEM_CONCERN,
                                                                       func))
        except Exception:
            raise
        return context_object

    def process_context(self):
        # returns true if all the child concerns and itself is resolved
        # check if all child concerns are resolved.
        # TODO
        def _process_context(context_objs, unresolved, visited_ids):
            if len(context_objs) == 0:
                return unresolved
            current_context_obj = context_objs[0]
            context_objs = context_objs[1:]
            if current_context_obj.id in visited_ids:
                return unresolved
            visited_ids.append(current_context_obj.id)
            # if current_context_obj.state != ContextObjectStates.RESOLVED:
            # only concerns can be marked as UNRESOLVED, so I can assume it's a concern that comes here.
            _unresolved = _process_context(current_context_obj.context, [], visited_ids)
            if len(_unresolved) == 0:
                self._resolve_concern(current_context_obj)
            if current_context_obj.state != ContextObjectStates.RESOLVED:
                unresolved.append(current_context_obj)
            unresolved += _unresolved
            return _process_context(context_objs, unresolved, visited_ids)
        _process_context([self._root], [], [])

    def _resolve_concern(self, context_obj):
        '''
        Return True if concern is/was succesflully resolved.
        '''
        if context_obj.state == ContextObjectStates.RESOLVED:
            return
        # if self._check_children(context_obj):
        if context_obj.function is not None:
            # the function resolver to basically outsource how the function is resolved.
            exec_success, outputs = self._function_resolver.resolve(context_obj.function,
                                                                    context_obj.context)
            # if not exec_success:
            #     # If the context does not have the expected results in the resolved context,
            #     # return true but say that the function execution was a dud
            #     # return True
            # else:
            # For now deligating the responsibility of announcing the error to the user to
            # the function_resolver
            if outputs is not None:
                for output in outputs:
                    context_obj.add_context(
                        self._create_context_object(output,
                                                    self._get_id(),
                                                    ContextObjectTypes.SYSTEM_RESPONSE,
                                                    None))
        else:
            exec_success = self._check_children(context_obj)
        if exec_success:
            context_obj.state = ContextObjectStates.RESOLVED
        return
        # else:
        #     return False

    def _check_children(self, context_obj):
        '''
        Return True if all children have been marked as resolved.
        '''
        for child_context_obj in context_obj.context:
            if child_context_obj.state == ContextObjectStates.UNRESOLVED:
                return False
        return True

    def _function_name_list_to_id_list(self, name_list):
        return_list = []
        unlisted_names = []
        for name in name_list:
            try:
                return_list.append(self._all_system_concerns_id_map[name])
            except KeyError:
                unlisted_names.append(name)
        return return_list, unlisted_names

    def _get_id(self):
        return_id = self._current_id
        self._current_id = self._current_id + 1
        return return_id


class Dialogue_Manager():
    def __init__(self, response_function_model, trigger_function_model, function_index, function_resolver):
        self.context = Context(function_index, function_resolver)
        self.response_function_model = response_function_model
        self.trigger_function_model = trigger_function_model
        self.function_index = function_index
        self.function_resolver = function_resolver

    def process_user_utterance(self, utterance):
        # Get the predicted response and trigger functions
        trigger_functions = self._get_trigger_functions(utterance)
        response_functions = self._get_response_function(utterance)

        # isConcern = len(trigger_functions) > 0
        self.context.add_user_context_object(utterance, response_functions, trigger_functions)
        self.context.process_context()

    def _get_trigger_functions(self, utterance):
        '''
        This function processes the utterance using the trigger_function_model and does any other
        filtering that is required. It will return the list of possible trigger functions related
        to the utterance. Returnes an empty list if there are not trigger function.
        '''
        # Models output
        trigger_functions = self.trigger_function_model(utterance)

        # Assumes the return value of the trigger_functions_model is a dictionaty
        # with the names of the functions it thinks are trigger functions and their
        # respective scores
        return [n for n, p in trigger_functions.items()]

    def _get_response_function(self, utterance):
        '''
        Returns the list of response functions for the utterance. It's is first processed through the
        response_function_model.
        Will raise DialogueProcessingException if the calculated response functions list is empty.
        '''
        response_functions = self.response_function_model(utterance)

        if len(response_functions) == 0:
            raise DialogueProcessingException("No response functions")
        return [n for n, p in response_functions.items()]

    def get_next_system_concern(self):
        active_concerns = self.context.get_active_concerns()
        active_concerns = sorted(active_concerns.items(), key=lambda x: x[0], reverse=True)
        try:
            return active_concerns[0][1].name
        except IndexError:
            return None

    def reset_context(self):
        self.context.reset_context()


class DialogueProcessingException(Exception):
    pass
