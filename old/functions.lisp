(load "dm.lisp")

(defun root-concern (&rest restx)
  (print "root concern resolved"))

(defun book-train (name destination departure number-of-tickets)
  (print "booked ticket to train"))

(defun order-taxi (location)
  (print "ordered taxi"))

(defun get-train-info (departure destination)
  (print "you will get the information someday"))

(defun order-food (food-type number-of-orders)
  (print "food type odered"))

(defun signup-online-account (name age email-address)
  (print "signed up"))

(defun book-room (name number-of-rooms number-of-days)
  (print "booked room"))

(defun reset-functions ()
  (clrhash *functions*)
  (add-new-function "root-concern" (symbol-function (find-symbol (string-upcase "root-concern"))))
  (add-new-function "book-train" (symbol-function (find-symbol (string-upcase "book-train"))))
  (add-new-function "order-taxi" (symbol-function (find-symbol (string-upcase "order-taxi"))))
  (add-new-function "get-train-info" (symbol-function (find-symbol (string-upcase "get-train-info"))))
  (add-new-function "order-food" (symbol-function (find-symbol (string-upcase "order-food"))))
  (add-new-function "signup-online-account" (symbol-function (find-symbol (string-upcase "signup-online-account"))))
  (add-new-function "book-room" (symbol-function (find-symbol (string-upcase "book-room")))))

(defun reset-dialogue ()
  (reset-context)
  (push (make-context-obj (list :s :c -1) :definition "root-concern"
					 :function (dm-function-function (gethash "root-concern" *functions*)))
        *process-history*))
