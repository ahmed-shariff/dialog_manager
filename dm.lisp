(load "context.lisp")
(load "model.lisp")
(asdf:load-system "alexandria")
(defvar *functions* (make-hash-table :test 'equalp))

(defvar *trigger-min-probability* 0.0)
(defvar *response-min-probability* 0.0)
(defvar *parameter-min-strength* 0.0 "when there are more than one response to a paramater extraction function,
the second value returned by the parameter extraction function provides the strength of the result")
(defvar *process-history-coefficient* 0.25)

(defclass dm-function ()
  ((name
    :initarg :name
    :initform (error "Must provide name")
    :reader dm-function-name)
   (function
    :initarg :function
    :initform (error "Must provide function")
    :accessor dm-function-function)
   (parameters
    :documentation "This will be a hashtable, where the key is the name of the function and the value either t or nil, 
    t if the relevent function had produced the result. This would be nil if this is a parameter extraction function
    not that the name of the function is not the name of the parameter of the function this object represent"
    :initarg :parameters
    :initform nil
    :accessor dm-function-parameters)
   (value
    :documentation "a value that is used to relate to the function"
    :initarg :function-value
    :initform 0
    :accessor dm-function-value)))

(defclass dm-result ()
  ((calling-function-name
    :documentation "the name of the function that produces this result"
    :initarg :fn-name
    :reader dm-result-fn-name)
   (return-value
    :initarg :return-value
    :reader dm-result-value)))

(defgeneric process-result-context-obj-definition (context-obj-definition))

(defmethod process-result-context-obj-definition (context-obj-definition)
  (values nil nil))

(defmethod process-result-context-obj-definition ((context-obj-definition dm-result))
  (values (dm-result-fn-name context-obj-definition) (dm-result-value context-obj-definition)))

(defun add-new-function (name function &optional (value 0) (parameters nil))
  (setf (gethash name *functions*)
	(make-instance 'dm-function
		       :name name
		       :function function
		       :function-value value
		       :parameters parameters)))

(defmethod initialize-instance :after ((dm-function-obj dm-function) &key)
  ;;this will setup the function object, which will be derived from the function passed
  (with-accessors ((name dm-function-name)
		   (fn dm-function-function)
		   (parameters dm-function-parameters)
		   (value dm-function-value))
      dm-function-obj
    (let ((arglist (swank-backend:arglist fn)))
      (setf parameters '())
      (unless (equalp fn #'extract-paramater)
	    (dolist (parameter arglist)
	      (let ((parameter-name (concatenate 'string  name "-" (string-downcase (string parameter)))))
		(push parameter-name parameters);;here i can have a function to check the context for possible answers
		(add-new-function parameter-name
				  #'extract-paramater)))));;get paramater functon, that is parameter extraction
    (let ((passed-fn fn))
      (block function-block
	(setf fn #'(lambda (context id)
		     (if (null context)
			 (progn
			   (dolist (parameter-name parameters)
			     (push (make-context-obj (list :s :c id)
						     :definition parameter-name
						     :function (dm-function-function (gethash parameter-name *functions*)))
				   context))
			   (values nil nil))
			 (if (null parameters)
			     (let ((sorted-responses (sort (loop for subc-context in context
								 collect (funcall passed-fn (context-obj-definition subc-context) value))
							   ;;the passed-fn here will be returning a two value list
							   ;;first the return value, second the strength of the reply
							   #'> :key #'second)))
			       (if (< *parameter-min-strength* (second (first sorted-responses)))
				 (values :executed (make-instance 'dm-result
								  :fn-name name
								  :return-value (first (first sorted-responses))))
				 (values nil nil)))
			     ;;handlethe case where there are multiple parameters
			     (let ((result-list (loop named outer for arg in (swank-backend:arglist passed-fn)
						      collect (loop named inner for subc-context-obj in context
								    do (multiple-value-bind (fn-name fn-result)
									   (process-result-context-obj-definition (context-obj-definition
														   subc-context-obj))
									 ;(print "WOW")
									 (if (and (not (null fn-name))
										  (search (string arg) fn-name :test #'equalp))
									     (return-from inner fn-result)
									     (return-from outer nil)))))))
			       (if result-list
				 (let (result (apply passed-fn result-list))
				   (values :executed (make-instance 'dm-result
								    :fn-name name
								    :return-value result)))
				 (values nil nil)))))))))))

(defun extract-paramater (utt-string value)
  (list utt-string 1))

(defun set-parameters (fn)
  (let ((arglist (swank-backend:arglist fn)))
    (if (null (cdr arglist))
	();;parameter extraction
	())))

;;functions to be used in context.lisp
(defun definition-fn (utterence-string active-concerns process-history)
  (destructuring-bind (response-list trigger-list)
      (get-fn utterence-string)
    (let* ((return-val-1
	    (if (or (equalp (car (first trigger-list)) "nil")
		    (< (cdr (first trigger-list)) *trigger-min-probability*))
			    :r
			    :c))
	  (return-val-2
	    (let* (;;get the list of names of functions from the active concern list
		   (active-concern-fn-list (mapcar #'context-obj-definition active-concerns))
		   ;;get the list of names and corresponding probability of functions that are in the active concerns
		   (response-short-list (loop for fn-name in response-list
					      ;do (print *response-min-probability*)
					      when (and (> (cdr fn-name) *response-min-probability*)
							(member (car fn-name) active-concern-fn-list :test #'string-equal))
						collect fn-name
					      end))
		   ;;calculate the weited value for each function
		   ;;the weighted value is the probability into a weight based on how old it is on the process-history list
		   (response-calculated-list (loop for p in process-history
						   for i = 0 then (+ i 1)
						   ;do (print (assoc (context-obj-definition p) response-short-list :test #'equalp))
						   ;   (print (context-obj-definition p))
						   when (and (eql :c
								  (second (context-obj-type p)))
							     (assoc (context-obj-definition p) response-short-list :test #'equalp))
						     collect (cons (context-obj-definition p)
								   (list (/ 1 (+ 1 (* i *process-history-coefficient*)))
									 (context-obj-id p)))
						   
						   end)))
	      ;;get the refference number of the function with the highest weighted value
	      ;(print response-list)
	      ;(print active-concern-fn-list)
	      (loop for (fn-name fn-name-calculated-val fn-ref) in response-calculated-list
		    for max-fn = fn-name-calculated-val then (if (> fn-name-calculated-val max-fn)
					    fn-name-calculated-val
					    max-fn)
		    and max-fn-ref = fn-ref then (if (> fn-name-calculated-val max-fn)
						fn-ref
						max-fn-ref)
			     collect (list max-fn max-fn-ref) into a
		    finally (return max-fn-ref))))
	   (return-val-3 (if (eql :c return-val-1)
			     trigger-list
			     utterence-string)))
      (values return-val-1 return-val-2 return-val-3))))

(defun find-context-function (context-obj)
  (let ((dm-function-object (gethash (car (first (context-obj-definition context-obj))) *functions*)))
    (values (dm-function-function dm-function-object)
	    (dm-function-name dm-function-object))))


(defun turn-user (utterence-string)
  (process-user-utterence utterence-string #'definition-fn)
  (process-system-turn #'find-context-function))

(defun reset-context ()
  (setf *context* '())
  (setf *context-obj-list* '())
  (setf *id* -1)
  (setf *active-concerns* '())
  (setf *process-history* '()))
