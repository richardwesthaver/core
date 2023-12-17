;;; lib/obj/id.lisp --- IDs
(in-package :obj/id)

(defclass id ()
  ((id :initarg :id :initform 0 :accessor id-of :type fixnum)))

(defgeneric reset-id (obj)
  (:documentation "Reset the id slot of SELF to 0.")
  (:method ((obj standard-object)) (setf (id-of obj) 0))
  (:method ((obj t)) 0))

(defgeneric update-id (obj)
  (:documentation "Update the id slot of SELF.")
  (:method ((obj standard-object)) (setf (id-of obj) (hash-object obj)))
  (:method ((obj t)) (hash-object obj)))

(defgeneric make-id (&optional obj)
  (:documentation "Make a new ID object of a specified KIND.")
  (:method (&optional (obj t)) (make-instance 'id :id (or obj 0))))
