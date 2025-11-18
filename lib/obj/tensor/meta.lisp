;;; meta.lisp --- Tensor MOP

;; 

;;; Commentary:

;; [[id:0521332c-11d2-4ffc-8ada-99690b8b2655][dispatch strategy for tensor methods]]

;; DEFTENSOR - define a tensor object class

;; DEFINE-TENSOR-METHOD - define a tensor method

;;; Code:
(in-package :obj/tensor)

(defclass tensor-class (standard-class) ())

(defmacro deftensor (name supers slots &rest options)
  `(defclass ,name ,supers ,slots ,@options (:metaclass tensor-class)))

(defclass tensor-method-generator (standard-generic-function) ()
  (:metaclass funcallable-standard-class))


(defclass classp-specializer (specializer)
  ((object-class :initform nil :initarg :object-class)
   (direct-methods :initform nil :reader specializer-direct-methods))
  (:documentation "Exact class specializer."))
(defmethod print-object ((obj classp-specializer) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ", ~a" (class-name (slot-value obj 'object-class)))))

(defmethod add-direct-method ((specializer classp-specializer) method)
  (pushnew method (slot-value specializer 'direct-methods)))
(defmethod remove-direct-method ((specializer classp-specializer) method)
  (setf (slot-value specializer 'direct-methods)
        (remove method (slot-value specializer 'direct-methods))))
(defmethod make-load-form ((obj classp-specializer) &optional env)
  (declare (ignore env))
  (values `(classp-specializer ',(class-name (slot-value obj 'object-class))) nil))

(defclass group-specializer (specializer)
  ((object-class :initform nil :initarg :object-class)
   (group-name :initform nil :initarg :group-name)
   (direct-methods :initform nil :reader specializer-direct-methods))
  (:documentation "Applicable for each group-specializer with distinct
GROUP-NAME, the classes of the respective argument are the same."))
(defmethod print-object ((obj group-specializer) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ", ~a, ~a" (class-name (slot-value obj 'object-class)) (slot-value obj 'group-name))))

(defmethod add-direct-method ((specializer group-specializer) method)
  (pushnew method (slot-value specializer 'direct-methods)))
(defmethod remove-direct-method ((specializer group-specializer) method)
  (setf (slot-value specializer 'direct-methods)
        (remove method (slot-value specializer 'direct-methods))))
(defmethod make-load-form ((obj group-specializer) &optional env)
  (declare (ignore env))
  (values `(group-specializer ',(class-name (slot-value obj 'object-class)) ',(slot-value obj 'group-name)) nil))
;;Subtype
(defclass subtype-specializer (specializer)
  ((specializer-type :initform nil :initarg :specializer-type)
   (direct-methods :initform nil :reader specializer-direct-methods))
  (:documentation "Applicable only if for each group-specializer with distinct @argument{group-name}, the classes of the respective argument are the same."))
(defmethod print-object ((obj subtype-specializer) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream ", ~a" (slot-value obj 'specializer-type))))

(defmethod add-direct-method ((specializer subtype-specializer) method)
  (pushnew method (slot-value specializer 'direct-methods)))
(defmethod remove-direct-method ((specializer subtype-specializer) method)
  (setf (slot-value specializer 'direct-methods)
        (remove method (slot-value specializer 'direct-methods))))
(defmethod make-load-form ((obj subtype-specializer) &optional env)
  (declare (ignore env))
  (values `(subtype-specializer ',(slot-value obj 'specializer-type)) nil))

(defmacro define-tensor-method (name args &body body))
