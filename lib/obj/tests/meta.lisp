;;; meta.lisp --- Metaclass Tests

;; 

;;; Code:
(in-package :obj/tests)

;;;; Fast

(deftest fast ()
  (is= 42 (%test-+ 2 40)))

;;;; Dynamic
(defclass dyno1 (id)
  ((id :dynamic t :accessor id))
  (:metaclass dynamic-class))

(deftest dynamic-class ()
  (let ((obj (make-instance 'dyno1 :id 1)))
    (slot-dvar* obj 'id)
    (slot-dlet (((obj 'id) 0))
      (iszero (id:id obj)))
    (is= 1 (id:id obj))))

;;;; Stealth
(defclass stealth-target () ())

(deftest stealth-mixin ()
  (add-mixin 'id 'stealth-target)
  (issubclass 'id 'stealth-target)
  (define-stealth-mixin stealth-mixer (secret-object) stealth-target
    ())
  (issubclass 'secret-object 'stealth-mixer)
  (issubclass 'secret-object 'stealth-target)
  (issubclass 'stealth-mixer 'stealth-target))

;;;; Filtered
(defmethod fac ((n number))
  (* n (fac (- n 1))))

(defmethod fac ((n (eql 0)))
  1)

(deftest filtered-function ()
  (is= 3628800 (fac 10)))

;;;; Stored
(defsclass person ()
  ((name :accessor name :initarg :name :transient t)
   (id :accessor id :initarg :age)
   (father :accessor father :initarg :father)
   (school :accessor school :initarg :school)))

(defsclass school ()
  ((name :accessor name :initarg :name :allocation :instance))
  (:schemas t))

(defclass school-schema (store:stored-object-schema) ()
  (:default-initargs
   :class-name 'school
   :version 1))

;; (get-class-indexing (find-class 'school))
;; (get-store-schemas (find-class 'school))
(defvar *test-store* (make-instance 'store))

(deftest stored (:skip t)
  (with-transaction (txn)
    (mapcar #'(lambda (initargs) (apply #'make-instance 'school initargs))
            '((:name "West Side")
              (:name "Fitch")
              (:name "Cutler")))
    (mapcar #'(lambda (initargs) (apply #'make-instance 'person initargs))
            `((:name "Bob" :age 40 :father nil 
               :school ,(get-instance-by-value 'school 'name "Cutler"))))
    (mapcar #'(lambda (initargs) (apply #'make-instance 'person initargs))
            `((:name "Fred" :age 20 :father nil 
               :school ,(get-instance-by-value 'school 'name "West Side"))
              (:name "Sally" :age 30 :father ,(get-instance-by-value 'person 'name "Bob")
               :school ,(get-instance-by-value 'school 'name "Fitch"))
              (:name "George" :age 50 :father ,(get-instance-by-value 'person 'name "Bob")
               :school ,(get-instance-by-value 'school 'name "Cutler"))))))
