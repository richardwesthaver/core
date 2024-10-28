;;; meta/indexed.lisp --- Indexed Metaclasses

;;

;;; Code:
(in-package :obj/meta/indexed)

(defclass indexed-class (standard-class) ())

(defmethod validate-superclass ((class standard-class) (superclass indexed-class)) t)
(defmethod validate-superclass ((class indexed-class) (superclass standard-class)) t)

(defmethod find-slot-defs-by-type ((class indexed-class) type &optional (by-subtype t))
  (let ((slot-defs (sb-mop:class-slots class)))
    (loop for slot-def in slot-defs
         when (if by-subtype
                  (subtypep (type-of slot-def) type)
                  (eq (type-of slot-def) type))
         collect slot-def)))

(defclass indexed-slot-definition ()
  ((indexed :accessor indexp :initarg :indexed :initarg :index :initform nil :allocation :instance)
   (inherit :accessor inheritp :initarg :inherit :initform nil :allocation :instance)))

(defclass indexed-direct-slot-definition (indexed-slot-definition)
  ())

(defclass indexed-effective-slot-definition (indexed-slot-definition)
  ((indices :accessor indexed-slot-indices :initform nil :allocation :instance
            :documentation "Alist of actual indices by store")
   (base-class :accessor indexed-slot-base :initarg :base-class :allocation :instance
               :documentation "The base class to use as an index")))

(defmethod indexp (def)
  (declare (ignore def))
  nil)

(defmethod get-slot-def-index ((def indexed-effective-slot-definition) sc)
  (awhen (assoc sc (indexed-slot-indices def))
    (cdr it)))

(defmethod add-slot-def-index (idx (def indexed-effective-slot-definition) sc)
  (setf (indexed-slot-indices def)
        (acons sc idx (indexed-slot-indices def))))

(defmethod clear-slot-def-index ((def indexed-effective-slot-definition) sc)
  (setf (indexed-slot-indices def)
        (remove sc (indexed-slot-indices def) :key #'car)))

(defmethod indexed-slot-defs (class)
  (find-slot-def-names-by-type class 'indexed-effective-slot-definition nil))

(defmethod indexed-slot-names (class)
  (find-slot-def-names-by-type class 'indexed-effective-slot-definition nil))
