(load "model.lisp")
(load "functions.lisp")
(load "driver.lisp")
(asdf:load-system :mgl)
(asdf:load-system :stem)
(asdf:load-system :burgled-batteries)
(asdf:load-system :alexandria)

(defparameter *trigger-result-list* '(0 0) "first is the correct readings, second is the total")
(defparameter *response-result-list* '(0 0) "first is the correct readings, second is the total")
(defparameter *tested-utterances* '(0 0))
(defparameter *stream* t)
(defparameter *use-model* :maxent);; :nb or :maxent
(defparameter *stem* nil)


(defun reset-experiment-result ()
  (setf (first *trigger-result-list*) 0
	(second *trigger-result-list*) 0
	(first *response-result-list*) 0
	(second *response-result-list*) 0))


(defun calculate-accuracy-basic (test-set training-set)
  (reset-model)
  (reset-experiment-result)
      ;(print *trigger-result-list*)
					;(print *response-result-list*)
  (case *use-model*
    (:nb (progn
	   (format *stream* "~%~%*********Training with Naive-bayes model*********~%")
	   (dolist (doc training-set)
	     (apply #'add-utterence doc))
	   (train-resolving-fn *fn-list*)
	   (train-triggering-fn *fn-list*)))
    (:maxent (progn
	       (format *stream* "~%~%*********Training with Maxent model*********~%")
	       (train-maxent-model training-set))))
  ;(print test-set)
  (dolist (doc test-set)
    ;(print doc)
    (destructuring-bind (response-list trigger-list)
        (get-fn (first doc))
      ;(print response-list)
      ;(print  trigger-list)
      (incf (second *trigger-result-list*))
      (incf (second *response-result-list*))
      (when (string-equal (third doc) (car (first response-list)))
	(incf (first *trigger-result-list*)))
      (when (string-equal (second doc) (car (first trigger-list)))
	(incf (first *response-result-list*)))))
    ;(print *trigger-result-list*)
					;(print *response-result-list*)
  (format *stream* "New run")
  (format *stream* "~%Accuracy of trigger functions: ~a    returned values: ~a"
	  (/ (first *trigger-result-list*) (second *trigger-result-list*)) *trigger-result-list*)
  (format *stream* "~%Accuracy of response functions: ~a  returned values: ~a~%"
	  (/ (first *response-result-list*) (second *response-result-list*)) *response-result-list*)
  (append *trigger-result-list* *response-result-list*))

(defun extract-conversations (document)
  (let ((conversation-collection '())
	(conversation '()))
    (dolist (utterence document)
      ;(print utterence)
      (when (string-equal "root-concern" (third utterence))
	(push (reverse conversation) conversation-collection)
	(setf conversation '()))
      ;(print conversation)
      (push utterence conversation))
    (remove nil conversation-collection)))

(defun calculate-accuracy-with-conversations (test-set-conversations training-set-conversations)
  (reset-model)
  (reset-experiment-result)
  (reset-functions)
  ;;(print test-set-conversations)
  (incf (second *tested-utterances*) (length (apply #'append test-set-conversations)))
  (let ((training-set (apply #'append training-set-conversations)))
    (case *use-model*
      (:nb (progn
	     (format *stream* "~%~%*********Training with naive-bayes model*********~%")
	     (dolist (doc training-set)
	       (apply #'add-utterence doc))
	     (train-resolving-fn *fn-list*)
	     (train-triggering-fn *fn-list*)))
      (:maxent (progn
	       (format *stream* "~%~%*********Training with Maxent model*********~%")
	       (train-maxent-model training-set)))))
  (dolist (conversation test-set-conversations)
    (reset-dialogue)
    ;(print conversation)
    (process-conversation (first conversation) conversation (length conversation)))
  (format *stream* "~%~%New run")
  (format *stream* "~%Accuracy of trigger functions: ~a"  (/ (first *trigger-result-list*) (second *trigger-result-list*)))
  (format *stream* "~%Accuracy of response functions: ~a" (/ (first *response-result-list*) (second *response-result-list*)))
  (append *trigger-result-list* *response-result-list*))
  


(defun process-conversation (utterence conversation count)
  (unless (eql count 0)
    (let* ((next-system-concern (turn-user (first utterence)))
	  (last-user-context-obj (find :u *process-history*
				       :test #'equalp
				       :key #'(lambda (context-obj)
						(first (context-obj-type context-obj))))))      
					;(print "WOW***********************************")
      (print utterence)
      (print next-system-concern)
      ;(print (context-obj-definition last-user-context-obj))
      (incf (second *trigger-result-list*))
      (incf (second *response-result-list*))
      (incf (first *tested-utterances*))
      (let ((definition (context-obj-definition last-user-context-obj))
	    (parent-id (third (context-obj-type last-user-context-obj)))) 
	(if (listp definition)
	    (when (string-equal (second utterence) (car (first definition)))
	      (incf (first *trigger-result-list*)))
	    (when (string-equal "nil" (second utterence))
	      (incf (first *trigger-result-list*))))
	(when (string-equal (get-sys-concern-fn-name (context-obj-definition (nth parent-id *context-obj-list*)))
			    (third utterence))
	  (incf (first *response-result-list*))))
      (if (find next-system-concern conversation
			:test #'string-equal
			:key #'third)
	  (process-conversation (find next-system-concern conversation
				      :test #'string-equal
				      :key #'third)
				conversation
				(decf count))
	  (print "FAAAAIIILLLLL>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")))))

	  ;(progn
	    ;(incf (second *response-result-list*) (- count 1))
	    ;(incf (second *trigger-result-list*)))))))


(defmethod get-sys-concern-fn-name ((definition dm-result))
  (dm-result-fn-name definition))
(defmethod get-sys-concern-fn-name (definition)
  definition)

;;***************************************************************************
;;above are functions that does calculations alone, below are functions that does pre-processing and pass to above functions

(defun avg-accuracy (result)
  (let ((trigger-avg '(0 0))
	(response-avg '(0 0)))
    (loop for single-result in result
	  counting t into total
	  summing (/ (first single-result) (second single-result)) into trigger-sum
	  summing (/ (third single-result) (fourth single-result)) into response-sum
	  finally
	     (format *stream*
		     "~%~%FINAL VALUES:~%trigger function average accuracy:  ~a~%response function average accuracy:  ~a"
		     (float (/ trigger-sum total))
		     (float (/ response-sum total)))
	     (return (list (float trigger-sum) (float response-sum) total)))))


;;(avg-accuracy (mgl-resample:bag-cv (extract-conversations *document*) #'calculate-accuracy-with-conversations :n 3 :n-folds 3))
;;(avg-accuracy (mgl-resample:bag-cv *document* #'calculate-accuracy-basic :n 3 :n-folds 3))
	
(defun basic-cv-calculate (document)
  (format *stream* "~%~%~%****Calculating accuracy for utterences****")
  (avg-accuracy (mgl-resample:cross-validate (if *stem* (stem-document document) document)
					     #'calculate-accuracy-basic 
					     :n-folds 5 
					     :split-fn #'(lambda (seq fold n-folds)
							   (mgl-resample:split-stratified seq fold n-folds
											  :key #'third
											  :test #'string-equal)))))

(defun conversation-cv-calculate (document)
  (setf (first *tested-utterances*) 0
	(second *tested-utterances*) 0)
  (format *stream* "~%~%~%****Calculating accuracy for conversations****")
  (avg-accuracy (mgl-resample:cross-validate (extract-conversations (if *stem* (stem-document document) document))
					     #'calculate-accuracy-with-conversations
					     :n-folds 5
					     :split-fn #'(lambda (seq fold n-folds)
							   (mgl-resample:split-stratified seq fold n-folds
											  :key #'(lambda (element)
												   (second (first element)))
											  :test #'string-equal))))
  (format *stream* "~%~%Tested utterances: ~a" (apply #'/ *tested-utterances*)))

(defun experiment-whole-doc-conv-cv ()
  (conversation-cv-calculate *document*))

(defun experiment-whole-doc-basic-cv ()
  (basic-cv-calculate *document*))

(defun stem-document (document)
  (loop for doc in document
	collect (append (list (concatstring (loop for str in (cl-utilities:split-sequence #\space (first doc))
						  collect (stem:stem str))))
			(cdr doc))))

(defmacro write-to-file (execution file-name &key (if-exist :supersede))
  `(with-open-file (*stream* ,file-name :direction :output :if-exists ,if-exist)
     ,execution))


(defun full-test-suit ()
  (let ((*stem* nil))
    (let ((*use-model* :nb))
      (let ((*use-nil-store* nil))
	(write-to-file (experiment-whole-doc-basic-cv) "output_nb_no_nil.txt")
	(write-to-file (experiment-whole-doc-conv-cv) "output_nb_no_nil.txt" :if-exist :append))
      (let ((*use-nil-store* t))
	(write-to-file (experiment-whole-doc-basic-cv) "output_nb_with_nil.txt")
	(write-to-file (experiment-whole-doc-conv-cv) "output_nb_with_nil.txt" :if-exist :append)))
    (let ((*use-model* :maxent))
      (let ((*use-nil-store* nil))
	(write-to-file (experiment-whole-doc-basic-cv) "output_maxent_no_nil.txt")
	(write-to-file (experiment-whole-doc-conv-cv) "output_maxent_no_nil.txt" :if-exist :append))
      (let ((*use-nil-store* t))
	(write-to-file (experiment-whole-doc-basic-cv) "output_maxent_with_nil.txt")
	(write-to-file (experiment-whole-doc-conv-cv) "output_maxent_with_nil.txt" :if-exist :append))))
  (let ((*stem* t))
    (let ((*use-model* :nb))
      (let ((*use-nil-store* nil))
	(write-to-file (experiment-whole-doc-basic-cv) "output_nb_no_nil_stemed.txt")
	(write-to-file (experiment-whole-doc-conv-cv) "output_nb_no_nil_stemed.txt" :if-exist :append))
      (let ((*use-nil-store* t))
	(write-to-file (experiment-whole-doc-basic-cv) "output_nb_with_nil_stemed.txt")
	(write-to-file (experiment-whole-doc-conv-cv) "output_nb_with_nil_stemed.txt" :if-exist :append)))
    (let ((*use-model* :maxent))
      (let ((*use-nil-store* nil))
	(write-to-file (experiment-whole-doc-basic-cv) "output_maxent_no_nil_stemed.txt")
	(write-to-file (experiment-whole-doc-conv-cv) "output_maxent_no_nil_stemed.txt" :if-exist :append))
      (let ((*use-nil-store* t))
	(write-to-file (experiment-whole-doc-basic-cv) "output_maxent_with_nil_stemed.txt")
	(write-to-file (experiment-whole-doc-conv-cv) "output_maxent_with_nil_stemed.txt" :if-exist :append)))))
  
  
    
