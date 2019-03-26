(defvar *context* '())
(defvar *context-obj-list* '())
(defvar *id* -1)
(defvar *active-concerns* '());;contains all the active system concerns function
(defvar *process-history* '());; contains the process histroy, the top most being the latest

(defclass context-obj ()
  ((context-obj-type
    :documentation "a con cell
    first should be either :u or :s
    second should be either :c or :r
    third sould be an integer refering to which this was provided"
    :initarg :context-obj-type
    :initform (error "context-obj type must be a provided")
    :reader context-obj-type)   
   (context-obj-context
    :documentation "contains a list of context-object"
    :initarg :context-obj-context
    :initform '()
    :accessor context-obj-context)
   (id
    :initform (incf *id*)
    :reader context-obj-id)
   (context-obj-state
    :documentation
    "possible states:
    - :unresolved
    - :resolved"
    :initarg :context-obj-state
    :initform :unresolved
    :accessor context-obj-state)
   (context-obj-definition
    :documentation "The definition of the object relative to it's context
if it is a user context object it will contain a definition that will be used by the system
if it is a user concern, it will contain an alit of possible function's names with the probabilities
if it is a system concern, it will contain the function related to this concern, or the resolved definition(result) of the function"
    :initarg :context-obj-definition
    :initform nil
    :accessor context-obj-definition)
   (context-obj-function
    :documentation "A function that will be executed when all sub contexts are resolved
    it must take a context-obj as the paramater, which would be this context-object"
    :initarg :context-obj-function
    :initform nil
    :accessor context-obj-function)))

(defmacro make-context-obj (type &key definition function state context)
  "Utility function to simplyfy the creation of a new context-obj object"
  `(let ((utt (make-instance 'context-obj :context-obj-type ,type
			     ,@(if definition `(:context-obj-definition ,definition))
			     ,@(if function `(:context-obj-function ,function))
			     ,@(if state `(:context-obj-state ,state))
			     ,@(if context `(:context-obj-context ,context)))))
     (add-to-context utt)))

(defun add-to-context (context-obj)
  "context-obj - the context-obj object
   The object will be added to the *context-obj-list*, and added to the appropriate node in the *context*, 
   also if it is a system concern, it will add it to the active concerns"
  (push-at-end context-obj *context-obj-list*)
  (let ((parent-context-obj-id (third (context-obj-type context-obj)))) 
    (if (>= parent-context-obj-id 0)
	(push-at-end context-obj (context-obj-context (nth parent-context-obj-id *context-obj-list*)))
	(push-at-end context-obj *context*))
    (when (and
	   (eql :s (first (context-obj-type context-obj)))
	   (eql :c (second (context-obj-type context-obj))))
      (push context-obj *active-concerns*))
    context-obj))

(defmacro push-at-end (obj list)
  "Utility function to add an object to the end of the list"
  `(setf ,list (append ,list (list ,obj))))

(defun update-context-obj-state (context-obj)
  "update the state of the context-obj if it is unresolved.
For a context-obj to be resolved, all it's child objects must be resolved, also
if there is a context-obj-fn attached to it, that function must return :executed.
once a concern is resolved, it will be taken off the *active-concerns* list.
Note that a response object is always a leaf object. for a concern, if it has
concerns as it's children, they need to be resolved."
  (with-accessors ((type context-obj-type)
		   (state context-obj-state)
		   (context context-obj-context)
		   (fn context-obj-function)
		   (definition context-obj-definition)
		   (id context-obj-id))
      context-obj
    ;(format t "updating ~a~%" definition)
    (flet ((resolve-fn ()
	     ;(print "executing resolve-fn")
	     ;(print definition)
	     (if (eql fn nil)
		 (setf state :resolved)
		 (multiple-value-bind (fn-state fn-result)
		     (funcall fn context id)
		   (when (eql :executed fn-state)
		     (setf state :resolved)
		     (setf definition fn-result))
		   (when (and (eql state :resolved)
			      (member context-obj *active-concerns*))
		     (setf *active-concerns* (remove context-obj *active-concerns*)))))))
      (if (eql (second type) :r)
	  (resolve-fn)
	  (if context
	      (let ((unresolved-context-p (loop for utt in context
						do (update-context-obj-state utt)
						unless (eql (context-obj-state utt) :resolved)
						  collect utt
						end)))
		;(dolist (a unresolved-context-p)
		;  (format t "~a ::: ~a ~%" (context-obj-definition a) definition))
		(when (null unresolved-context-p)
		  (resolve-fn)))
	      (resolve-fn))))))

(defun process-user-utterence (utterence-string definition-fn)
  "The utterence in string from is the first parameter
definition-fn: returns either :c (concern) or :r (response) as first and an integer as second and returns a relevent definition object as final
An user context object is created and inculded into the context"
  (multiple-value-bind (type-2 refference definition)
      (funcall definition-fn utterence-string *active-concerns* *process-history*) 
    (let ((utt (make-context-obj (list :u type-2 refference)
				 :definition definition)))
      (push utt *process-history*))))

(defun process-system-utterence ()
  "Generates the system utterence, that is the next concern that needs to be addressed will be generated by this function"
  (let ((current-concern (first *active-concerns*)))
    (push current-concern *process-history*)
    ;;generate the response for the concern
					;(format t "~a~%" (context-obj-definition current-concern))
    ;;(print "THE OUTPUT IS:")
    (context-obj-definition current-concern)))

(defun process-system-turn (find-context-function)
  "When the last user utterence is a concern it looks for the backend function that needs to be excuted and include it as a system concern"
  (let ((last-user-utterence (first *process-history*)))
    (when (eql :c (second (context-obj-type last-user-utterence)))
      ;;if it is a concern
      (multiple-value-bind (fn fn-name)
	  (funcall find-context-function last-user-utterence)
	(let ((refference (context-obj-id last-user-utterence)))
	  (make-context-obj (list :s :c refference)
			    :function fn
			    :definition fn-name))))
    (dolist (obj *context*)
      (update-context-obj-state obj)))
    (process-system-utterence))
