(asdf:load-systems "cl-utilities"
		   "cl-naive-bayes")
(asdf:load-system :burgled-batteries)
(asdf:load-system :alexandria)
(asdf:load-system :inferior-shell)


(defvar *fn-list* (make-hash-table :test 'equalp) "list of function-obj")
(defparameter *resolve-store* (nbayes:make-learned-store))
(defparameter *trigger-store* (nbayes:make-learned-store))
(defparameter *trigger-nil-store* (nbayes:make-learned-store))
(defparameter *use-nil-store* nil)
(defparameter *use-model* :nb);; :nb or :maxent 



(defun concatString (list)
  "A non-recursive function that concatenates a list of strings."
  (if (listp list)
      (with-output-to-string (s)
	(format s "~{~a~^ ~}" list))))

(defclass function-obj ()
  ((trigger-utterence-list
     :documentation "a list containing the utterences that will trigger this function"
     :initform `()
     :accessor fn-trigger-utt-list)
   (resolving-utterence-list
    :documentation "list of utterences that resolve this"
    :initform `()
    :accessor fn-resolve-utt-list)))


(defun add-utterence (utterence-string trigger-fn resolving-fn)
  (let ((seq (cl-utilities:split-sequence #\Space utterence-string)))
    (symbol-macrolet ((fn-trigger (gethash trigger-fn *fn-list*))
		      (fn-resolve (gethash resolving-fn *fn-list*)))
      (unless fn-trigger
	(setf fn-trigger (make-instance 'function-obj)))
      (symbol-macrolet ((fn-trigger-list (fn-trigger-utt-list fn-trigger)))
	(setf fn-trigger-list (append fn-trigger-list `(,seq))))
      (when resolving-fn
	(unless fn-resolve
	  (setf fn-resolve (make-instance 'function-obj)))
	(symbol-macrolet ((fn-resolve-list (fn-resolve-utt-list fn-resolve)))
	  (setf fn-resolve-list (append fn-resolve-list `(,seq))))))))


(defun train-resolving-fn (fn-list)
  (loop for fn-name being the hash-key in fn-list
	  using (hash-value fn)
	do (loop for utt in (fn-resolve-utt-list fn)
		 do (nbayes:learn-a-document *resolve-store* utt fn-name))))

(defun train-triggering-fn (fn-list)
  (if *use-nil-store*
      (train-triggering-fn-with-nil fn-list)
      (train-triggering-fn-without-nil fn-list)))

(defun train-triggering-fn-without-nil (fn-list)
  (loop for fn-name being the hash-key in fn-list
	  using (hash-value fn)
	do (loop for utt in (fn-trigger-utt-list fn)
		 do (nbayes:learn-a-document *trigger-store* utt fn-name))))

(defun train-triggering-fn-with-nil (fn-list)
  (loop for fn-name being the hash-key in fn-list
	  using (hash-value fn)
	do (progn
	     (if (string-equal fn-name "nil")
		 (loop for utt in (fn-trigger-utt-list fn)
		       do (nbayes:learn-a-document *trigger-nil-store* utt "nil"))
		 (loop for utt in (fn-trigger-utt-list fn)
		       do (progn
			    (nbayes:learn-a-document *trigger-store* utt fn-name)
			    (nbayes:learn-a-document *trigger-nil-store* utt "not-nil")))))))
	       

(defun get-fn-nb (utterence-string)
  (let ((seq (cl-utilities:split-sequence #\Space utterence-string)))
    (list 
     (nbayes:sort-category-with-post-prob *resolve-store* seq)
     (if *use-nil-store*
	 (let ((nil-test (nbayes:sort-category-with-post-prob *trigger-nil-store* seq)))
	   (if (string-equal (car (first nil-test)) "nil")
	       nil-test
	       (nbayes:sort-category-with-post-prob *trigger-store* seq)))
	 (nbayes:sort-category-with-post-prob *trigger-store* seq)))))

(defun get-fn (utterance-string)
  (case *use-model*
    (:nb (get-fn-nb utterance-string))
    (:maxent (get-fn-maxent utterance-string))))
				 
(defun reset-model ()
  (clrhash *fn-list*)
  (setf *resolve-store* (nbayes:make-learned-store))
  (setf *trigger-store* (nbayes:make-learned-store))
  (setf *trigger-nil-store* (nbayes:make-learned-store)))

;;************************Maxent************************************
(defun extract-features-tfidf(document count)
  "document is a list of utterences and it's class, that is
in each datapoint, the first is the utterence and second is the class"
  (multiple-value-bind (classes-freq terms-freq)
      (calculate-class-term-frequency document)
    (let ((classes-tfidf (make-hash-table :test 'equalp))
	  (total-classes (length (alexandria:hash-table-keys classes-freq))))
      (loop for class being the hash-key of classes-freq
	      using (hash-value terms)
	    ;do (print class)
	    ;   (print (apply #'max (alexandria:hash-table-values terms)))))))
	    for max-term-freq = (apply #'max (alexandria:hash-table-values terms))
	    do (symbol-macrolet ((class-pointer (gethash class classes-tfidf)))
		 (setf class-pointer '())
		 (loop for term being the hash-key of terms
			 using (hash-value frequency)
		       do(push (cons term
				     (* (tf-n frequency)
					(idf-t total-classes (length (gethash term terms-freq)))))
			       class-pointer))
		 (setf class-pointer (subseq (sort class-pointer #'> :key #'cdr) 0 (when (> (length class-pointer) count) count)))))
      ;(print (alexandria:hash-table-values classes-tfidf))
      classes-tfidf)))

(defun tf-n (frequency)
  frequency)

(defun tf-1 ()
  1)

(defun idf-1 ()
  1)

(defun idf-t (total-classes document-term-frequency)
  (log (/ total-classes document-term-frequency) 10))
    

(defun calculate-class-term-frequency (document)
  "hash table contains a list of classes, for each class, a hash-table on each
term which gives the total number of that term in that class"
  (let ((classes (make-hash-table :test 'equalp))
	(terms (make-hash-table :test 'equalp)))
    (dolist (utterance document)
      (let ((seq (cl-utilities:split-sequence #\space (string-downcase (first utterance))))) 
	(dolist (utterance-term seq)
	  (symbol-macrolet ((class (gethash (second utterance) classes))
			    (term (gethash utterance-term terms)))
	    (when (null term)
	      (setf term '()))
	    (pushnew (second utterance) term :test #'string-equal)
	    (when (null class)
	      (setf class (make-hash-table :test 'equalp)))
	    (symbol-macrolet ((term-f (gethash utterance-term class)))
	      (if (null term-f)
		  (setf term-f 1)
		  (incf term-f)))))))
					;(print (length (alexandria:hash-table-keys terms)))
    (values classes terms)))

(defun train-maxent-model-py-files (document model-name &optional (count 100))
  (let ((classes-tfidf (extract-features-tfidf document count)))
    (with-open-file (f (concatenate 'string model-name ".py") :direction :output :if-exists :supersede)
      (macrolet ((fout (&rest body) `(format f (concatenate 'string ,@body))))
	(fout "from pyModule.ME_class import ME~%")
	(fout "from nltk.classify import accuracy~%")
	(fout "from random import shuffle~%")
	(fout "import pickle~%")
	(fout model-name "_featureset = [")
	(fout (with-output-to-string (utt-string)
		(format utt-string "~{~a~^,~}"
			(mapcar #'(lambda (x)
				    (extract-utterance-features-training classes-tfidf x))
				document))))
	(fout "]~%")
	(let ((prefix1 (concatenate 'string model-name "_feature_mapping_temp"))
	      (prefix2 (concatenate 'string model-name "_feature_mapping")))
	  (fout prefix1 " = {}~%")
	  (fout (with-output-to-string (utt-string)
		     (format utt-string (concatenate 'string "~{" prefix1 "~a~%~}")
			     (apply #'append (mapcar #'(lambda (x)
							 (extract-utterance-features-for-function-training classes-tfidf x))
						     document)))))
	  (fout prefix2 "={}~%")
	  (fout "for index,[label,f] in enumerate(" prefix1 "):~%")
	  (fout "    " prefix2 "[label,f] = (index," prefix1 "[label,f])~%")
	  (fout "shuffle(" model-name "_featureset)~%")
	  (fout "train_set,test_set = " model-name "_featureset[10:]," model-name "_featureset[:10]~%")
	  (fout model-name "_model = ME(" prefix2 ")~%"))
	(fout model-name "_model.train(" model-name "_featureset)~%")
	(fout "print(accuracy(" model-name "_model.classifier,test_set))~%")
	(fout "save_classifier = open(\"" model-name ".pickle\",\"wb\")~%")
	(fout "pickle.dump(" model-name "_model, save_classifier)~%")
	(fout "save_classifier.close()~%")))))

(burgled-batteries:startup-python)
(burgled-batteries:run "import pyModule.ME_class as t")
(burgled-batteries:run "from importlib import util")



;(burgled-batteries:run "import pyModule.ME_test as test")
;(burgled-batteries:defpyfun "test.xx" (y))
(burgled-batteries:defpyfun "t.get_maxent_list" (fset-dict model-name))
;(burgled-batteries:shutdown-python)

(defun get-maxent-list (utterance model-name)
  (let ((seq (cl-utilities:split-sequence #\space utterance))
	(fset-dict (make-hash-table :test 'equalp)))
    (symbol-macrolet ((term-hash (gethash term fset-dict)))
      (dolist (term seq)
	(if (null term-hash)
	    (setf term-hash 1)
	    (incf term-hash))))
    ;(print (alexandria:hash-table-alist fset-dict))
    (let ((val (t.get_maxent_list fset-dict model-name)))
      ;(print val)
      (sort (map 'list #'(lambda (x) (cons (first x) (second x))) val)
	    #'>
	    :key #'cdr))))
  
      
      ;; (bbr "classifier_file = open(\"" model-name ".pickle\",\"rb\")")
      ;; (bbr "classifier = pickle.load(classifier_file)")
      ;; (bbr "classifier_file = close()")
      ;; (bbr "print(classifier._mappings)"))))

(defun extract-utterance-features-training (classes-tfidf utterance)
  (let ((seq (cl-utilities:split-sequence #\space (first utterance)))
	(predicate-feature-term-list '())
	(term-list (gethash (second utterance) classes-tfidf)))
    
    ;(print utterance)
    ;(print term-list)
    (dolist (term seq)
      (let ((term-con (find term term-list :test #'string-equal :key #'car)))
	(when term-con
	  (when (> (cdr term-con) 0)
	    (push (with-output-to-string (s)
		    (format s "\"~a\":~a" (car term-con) (cdr term-con)))
		  predicate-feature-term-list)))))
    ;(print predicate-feature-term-list)
    
    (with-output-to-string (s)
      (format s "({")
      (when predicate-feature-term-list
	(format s "~{~a~^,~}" predicate-feature-term-list))
      (format s "}, \"~a\")" (second utterance)))))

(defun extract-utterance-features-for-function-training (classes-tfidf utterance)
  (let ((seq (cl-utilities:split-sequence #\space (first utterance)))
	(predicate-feature-term-list '())
	(term-list (gethash (second utterance) classes-tfidf)))
    
    ;(print utterance)
    ;(print term-list)
    (dolist (term seq)
      (let ((term-con (find term term-list :test #'string-equal :key #'car)))
	(when term-con
	  (when (> (cdr term-con) 0)
	    (push (with-output-to-string (s)
		    (format s "[\"~a\",\"~a\"] = ~a" (second utterance) (car term-con) (cdr term-con)))
		  predicate-feature-term-list)))))
    predicate-feature-term-list))

(defun train-maxent-model (document)
  (train-maxent-model-py-files (mapcar #'(lambda (x)
					   (list (first x) (third x)))
				       document)
			       "response_model")
  (inferior-shell:run/nil "python2 response_model.py")
  (if *use-nil-store*
      (let ((trigger-only-doc '())
	    (nil-doc '()))
	(dolist (doc document)
	  (if (string-equal "nil" (second doc))
	      (push (list (first doc) "nil") nil-doc)
	      (progn
		(push (list (first doc) "not-nil") nil-doc)
		(push doc trigger-only-doc))))
					;(print nil-doc)
	(train-maxent-model-py-files (mapcar #'(lambda (x)
						 (list (first x) (second x)))
					     trigger-only-doc)
				     "trigger_model")
	(train-maxent-model-py-files nil-doc "nil_model")
	(inferior-shell:run/nil "python2 nil_model.py"))
	
      (train-maxent-model-py-files (mapcar #'(lambda (x)
					       (list (first x) (second x)))
					   document)
				   "trigger_model"))
  (inferior-shell:run/nil "python2 trigger_model.py"))


(defun get-fn-maxent (utterance-string)
  (list
   (get-maxent-list utterance-string "response_model")
   (if *use-nil-store*
       (let ((nil-val (get-maxent-list utterance-string "nil_model")))
	 (print utterance-string)
	 (print nil-val)
	 (if (string-equal (car (first nil-val)) "nil")
	     nil-val
	     (get-maxent-list utterance-string "trigger_model")))
       (get-maxent-list utterance-string "trigger_model"))))
   
    ;(print predicate-feature-term-list)

;; (defun extract-utterance-features (utterance)
;;   (let* ((seq (cl-utilities:split-sequence #\space (first utterance)))
;; 	 (predicate-feature-term-list '())
;; 	 (N (length seq)))
;;     (dolist (term seq)
;;       (push (with-output-to-string (s)
;; 	      (format s
;; 		      "\"~a\":~a"
;; 		      term
;; 		      (* (tf-n (count term seq :test #'string-equal))
;; 			 (idf-t ()))))))))
		      
	;(fout "m.train(30,\"lbfg\")~%")
	;(fout "m.save(\"test_model\")")))))

;; (defun train-maxent-model (document)
;;   (let ((classes-tfidf (extract-features-tfidf document 1)))
;;     (with-open-file (f "model.py" :direction :output :if-exists :supersede)
;;       (macrolet ((fout (string) `(format f ,string)))
;; 					;(burgled-batteries:startup-python)
;; 	(fout "import os~%")
;; 	(fout "os.chdir(\"/media/Programmes/Programmes/maxent/python\")~%")
;; 	(fout "from maxent import MaxentModel~%")
;; 	(fout "os.chdir(\"/media/Files/Research/Code\")~%")
;; 	(fout "m = MaxentModel()~%")
;; 	(fout "m.begin_add_event()~%")
;; 	(dolist (utterance document)
;; 	  (let ((seq (cl-utilities:split-sequence #\space (first utterance)))
;; 		(predicate-feature-term-list '())
;; 		(term-list (gethash (second utterance) classes-tfidf)))
;; 	    (dolist (term seq)
;; 	      (let ((term-con (find term term-list :test #'string-equal :key #'car)))
;; 		(when term-con
;; 		  (push (with-output-to-string (s)
;; 			  (format s "(\"~a\",~a)" (car term-con) (cdr term-con)))
;; 			predicate-feature-term-list))))
;; 	    (fout (with-output-to-string (a)
;; 		    (format a "context = [")
;; 		    (format a "~{~a~^,~}" predicate-feature-term-list)
;; 		    (format a "]~%"))))
;; 	  (fout (with-output-to-string (s)
;; 		  (format s "m.add_event(context,\"~a\",1)~%" (second utterance)))))
;; 	(fout "m.end_add_event()~%")
;; 	(fout "m.train(30,\"lbfg\")~%")
;; 	(fout "m.save(\"test_model\")")))))
	
    
;(burgled-batteries:shutdown-python)))
	    
 
