;;; lib/obj/id.lisp --- IDs
(in-package :obj/id)

(defclass id ()
  ((id :initarg :id :initform 0 :accessor id :type fixnum)))

(defgeneric reset-id (obj)
  (:documentation "Reset the id slot of SELF to 0.")
  (:method ((obj standard-object)) (setf (id obj) 0))
  (:method ((obj t)) 0))

(defgeneric update-id (obj)
  (:documentation "Update the id slot of SELF.")
  (:method ((obj standard-object)) (setf (id obj) (hash-object obj)))
  (:method ((obj t)) (hash-object obj)))

(defgeneric make-id (kind)
  (:documentation "Allocate a new ID object of a specified KIND.")
  (:method ((kind (eql nil)))
    (declare (ignore kind))
    (make-instance 'id))
  (:method ((kind (eql t)))
    (declare (ignore kind))
    (make-instance 'id :id most-positive-fixnum)))

(defmethod print-object ((obj id) stream)
  (print-unreadable-object (obj stream :type "ID")
    (format stream "~A" (id obj))))

(defclass id-factory () ())
