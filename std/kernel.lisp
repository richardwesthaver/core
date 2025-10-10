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
  "The current kernel.")

(defclass kernel-class (sb-mop:funcallable-standard-class)
  ()
  (:documentation "Standard kernel class."))

(defmethod sb-mop:validate-superclass ((class kernel-class)
                                       (super sb-mop:funcallable-standard-class))
  t)

(defmethod sb-mop:validate-superclass ((class kernel-class)
                                       (super standard-class))
  t)

(defclass kernel-object (sb-mop:funcallable-standard-object)
  ()
  (:metaclass kernel-class)
  (:documentation "Standard kernel object."))

(defmethod kernel ((self kernel-object))
  (sb-pcl::%funcallable-instance-fun self))

(defmethod print-object ((self kernel-object) stream)
  (multiple-value-bind (expr closure-p name) (function-lambda-expression self)
    (declare (ignore expr))
    (print-unreadable-object (self stream :type t)
      (format stream "~@[~A~]~@[ :closure ~A~]" name closure-p))))
            
(definline make-kernel (fn)
  "Return a new KERNEL-OBJECT and set the instance function to FN."
  (declare (function fn))
  (let ((fin (make-instance 'kernel-object)))
    (sb-mop:set-funcallable-instance-function fin (compile nil fn))
    fin))

(deftype kernel () 
  "A type which specifies kernels. A kernel may be a list which is interpreted as
a lambda expression, a symbol which names a function, or a compiled-function."
  '(or cons symbol function kernel-object))

(defun check-kernel ()
  "Check the current value of *KERNEL*, ensuring it is bound appropriately
according to the current thread (worker, pool, super). STORE-VALUE
restarts is provided. *KERNEL* is returned."
  ;; TODO 2025-04-21: 
  (or *kernel*
      (restart-case (error 'no-kernel-error)
        (store-value (value)
          :report "Assign a value to *KERNEL*."
          :interactive (lambda () (print "Kernel: ") (read *query-io*))
          (check-type value kernel)
          (setf *kernel* value))))
  *kernel*)

(defmacro defkernel (name supers slots &rest opts)
  "Like DEFCLASS but for the KERNEL-CLASS metaclass."
  (let ((k (find :kernel opts :key #'car)))
    `(progn
       (defclass ,name ,(or supers '(kernel-object)) ,slots (:metaclass kernel-class) . ,(removef opts k :test 'equalp))
       (defmethod shared-initialize :after ((self ,name) slot-names &key &allow-other-keys)
         (sb-mop:set-funcallable-instance-function self ',(cdr k))))))

(defkernel hook () ()
  (:documentation "Hooks are Kernel objects which call an instance-specific
collection of functions at a pre-arranged point in time."))

(defkernel value-hook (hook) 
  ((value :initform nil :initarg :value :accessor hook-value))
  (:kernel 
   (lambda (self item &rest args)
     (case item
       (:add (apply 'add-hook self args))
       (:remove (apply 'remove-hook self args))
       (t
        (let ((val (hook-value self)))
          (mapcar 
           (lambda (x) (apply 'funcall x args))
           (if item
               (getf val item)
               val)))))))
  (:documentation "A hook which pushes and pops functions from a VALUE slot."))

(defkernel key-hook (value-hook) ()
  (:default-initargs :value (make-hash-table))
  (:kernel 
   (lambda (self item &rest args)
     (case item
       (:add (apply 'add-hook self args))
       (:remove (apply 'remove-hook self args))
       (t
        (let ((val (hook-value self)))
          (mapcar 
           (lambda (x) (apply 'funcall x args))
           (if item
               (gethash item val)
               (let ((vals))
                 (maphash (lambda (k v) (declare (ignore k)) (push v vals)) val)
                 vals))))))))
  (:documentation "A hook which stores separate categories of hook functions in a hash-table. The
key of each record is a category name and the value is a list of functions."))

(defgeneric add-hook (hook function &key &allow-other-keys)
  (:documentation "Add a FUNCTION to HOOK. The hook is checked to see if FUNCTION is already
present, but this will only work if you pass a symbol instead of an actual
function which we can't check the name of.")
  (:method ((hook value-hook) function &key append (test #'eql))
    (if append
        (when (not (find function (hook-value hook) :test test))
          (appendf (hook-value hook) (list function)))
        (pushnew function (hook-value hook) :test test)))
  (:method ((hook key-hook) function &key name (test #'eql))
    (multiple-value-bind (val found) (gethash name (hook-value hook))
      (if found
          (let ((new val))
            (pushnew function new :test test)
            (setf (gethash name (hook-value hook)) new))
          (setf (gethash name (hook-value hook)) (list function)))))
  (:method ((hook key-hook) (function list) &key)
    (setf (gethash (car function) (hook-value hook)) (cdr function))))

(defgeneric remove-hook (hook function)
  (:documentation "Remove a FUNCTION from HOOK. This will only work on function symbols, not
functions themselves.")
  (:method ((hook value-hook) item)
    (removef (hook-value hook) item))
  (:method ((hook key-hook) item)
    (remhash item (hook-value hook))))

(defmacro defhook (name forms &key (class ''key-hook) documentation)
  "Define a new hook with NAME bound to a hook specified by CLASS and FORMS
  being a list where each element is passed to ADD-HOOK."
  (with-gensyms (val)
    `(defparameter ,name 
       (let ((,val (make-instance ,class)))
         (mapcar (lambda (x) (add-hook ,val x)) '(,@forms))
         ,val)
       ,@(or documentation))))
