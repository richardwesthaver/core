;;; lib/obj/id.lisp --- IDs

;;

;;; Code:
(in-package :obj/id)

(defglobal *global-id-table* (make-hash-table))

(defclass id ()
  ((id :initarg :id :initform 0 :accessor id :type fixnum)))

(defclass global-id (id) ())

(defun global-id (id)
  (gethash id *global-id-table*))

(defun global-id-value (obj)
  (gethash (id obj) *global-id-table*))

(defun update-global-id (obj)
  (setf (gethash (id obj) *global-id-table*) obj))

(definline reset-global-id (id)
  (remhash id *global-id-table*))

(defmethod initialize-instance :after ((self global-id) &rest args)
  (declare (ignore args))
  (update-global-id self))

(defmethod id (self) (hash-object-address self))

(defgeneric reset-id (obj)
  (:documentation "Reset the id slot of SELF to 0.")
  (:method ((obj standard-object)) (setf (id obj) 0))
  (:method ((obj t)) 0)
  (:method :before ((obj global-id)) (reset-global-id (id obj))))

(defgeneric update-id (obj)
  (:documentation "Update the id slot of SELF.")
  (:method ((obj standard-object)) (setf (id obj) (hash-object obj)))
  (:method ((obj t)) (hash-object obj))
  (:method :after ((obj global-id)) (update-global-id obj)))

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

(defgeneric identify (self)
  (:documentation "Return the identity of object SELF - usually meant for objects which don't
specialize on ID but should still sometimes return an ID."))
