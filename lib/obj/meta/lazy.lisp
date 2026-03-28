;;; obj/meta/cached.lisp --- Simple cached generic functions

;;

;;; Code:
(in-package :obj/meta/lazy)

(defgeneric generic-function-cache (gf)
  (:method ((gf generic-function)) nil))

(defgeneric method-cache (sm)
  (:method ((sm standard-method)) nil))

(defclass cached-function (standard-generic-function)
  ((cache :initarg nil :accessor generic-function-cache))
  (:metaclass funcallable-standard-class))

(defclass cached-method (standard-method)
  ((cache :reader method-cache)))

(defmethod initialize-instance :before ((method cached-method) &key qualifiers)
  ;; make sure our cache is initialized.
  (print qualifiers)
  (unless qualifiers
    (when-let ((gf-cache (generic-function-cache (method-generic-function method))))
      (setf (slot-value method 'cache) gf-cache)))
  (when-let ((pos (position :cache qualifiers)))
    (if-let ((c (nth (1+ pos) qualifiers)))
      (setf (slot-value method 'cache) c)
      (error "Cache qualifier is not followed by a valid cache designator in method ~S." method)))
  method)
          
#|
(defgeneric c1 (self) (:generic-function-class cached-function))
(defvar *cac* (make-hash-table))
(defmethod c1 :cache *cac* (self) t)
(c1 t)
|#
