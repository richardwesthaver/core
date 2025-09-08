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
  (unless qualifiers
    (when-let ((gf-cache (generic-function-cache (method-generic-function method))))
      (setf (slot-value method 'cache) gf-cache)))
  (when (member (first qualifiers) '(:before :after :around))
    (pop qualifiers))
  (when (eq (first qualifiers) :cache)
    (print (pop qualifiers))
    (unless qualifiers
      (error "Cache qualifier is not followed by a cache designator in method ~S." method))
    (unless (first qualifiers)
      (error "NIL is not a valid cache designator in method ~S." method))
    (setf (slot-value method 'cache)
          (pop qualifiers)
          qualifiers qualifiers)))

;; (defgeneric c1 (self) (:generic-function-class cached-function) (:method-class cached-method))
;; (defvar *cac* (make-hash-table))
;; (defmethod c1 :cache *cac* ((self t)) t)
