;;; kernel.lisp --- Kernels

;; Primitive Kernel Object Types

;;; Code:
(in-package :std/prim)

;;; Kernel
(defgeneric kernel (self))
(defgeneric (setf kernel) (new self))

(define-condition kernel-init-error (error) ()
  (:report
   "The kernel failed to initialize.")
  (:documentation
   "Error signaled when a kernel object fails to initialize."))

(define-condition no-kernel-error (error) ()
  (:report "invalid *KERNEL*")
  (:documentation
   "Error signaled when a kernel object is invalid."))

(defvar *kernel* nil
  "The current kernel, or nil.")

(defclass kernel-class (sb-mop:funcallable-standard-class)
  ()
  (:documentation "Standard kernel class."))

(defmethod sb-mop:validate-superclass ((class standard-class)
                                       (super kernel-class))
  t)

(defmethod sb-mop:validate-superclass ((class kernel-class)
                                       (super standard-class))
  t)

(defmethod sb-mop:validate-superclass ((class kernel-class)
                                       (super sb-mop:funcallable-standard-class))
  t)

(defclass kernel-object (sb-mop:funcallable-standard-object)
  ()
  (:metaclass kernel-class)
  (:documentation "Standard kernel object."))

(definline make-kernel (fn)
  "Return a new KERNEL-OBJECT and set the instance function to FN."
  (declare (function fn))
  (let ((fin (make-instance 'kernel-object)))
    (sb-mop:set-funcallable-instance-function fin (compile nil fn))
    fin))

(deftype kernel () 
  "A type which specifies kernels. A kernel may be a list which is interpreted as
a lambda expression, a symbol which names a function, or a compiled-function."
  '(or cons symbol compiled-function kernel-object))
(deftype kernel-function ()
  "A compiled function suitable for use as the funcallable slot of a kernel."
  '(compiled-function * *))

(defun check-kernel ()
  "Check the current value of *KERNEL*, ensuring it is bound appropriately
according to the current thread (worker, pool, super). STORE-VALUE
restarts is provided. *KERNEL* is returned."
  ;; TODO 2025-04-21: 
  (or *kernel*
      (restart-case (error 'no-kernel-error)
        (store-value (value)
          :report "Assign a value to *KERNEL*."
          :interactive (lambda () (print "Value: ") (read t ))
          (check-type value kernel)
          (setf *kernel* value))))
  *kernel*)
