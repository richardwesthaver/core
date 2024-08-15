;;; skel/core/vm.lisp --- The Skel Virtual Machine

;; Stack slots refer to objects. a Stack is a sequence of objects
;; which can be output to a stream using a specialized function.

;;; Code:
(in-package :skel/core/vm)

(eval-always
  (defvar *skel-op-types*
    (vector :nil :eval :set :get :end :jump :pop :spawn :wait :print :let))
  (defvar *skel-arena-size* (ash 1 16))
  (defvar *skel-stack-size* 128)
  (defun new-skel-arena () (sb-vm:new-arena *skel-arena-size*))
  (defun init-skel-scope (&optional (map (sb-lockless:make-so-map/fixnum)))
    (sb-lockless:so-insert map 0)
    (sb-lockless:so-insert map 1)
    (sb-lockless:so-insert map 2)
    map))

(defun init-skel-value-scope (scope &rest values)
  (sb-lockless:so-insert
   scope 1
   (apply #'vector values)))

(defun init-skel-function-scope (scope &rest functions)
  (sb-lockless:so-insert
   scope 2
   (apply #'vector functions)))
    
(defvar *skel-arena* (new-skel-arena))

(deftype skel-op-type () `(member ,@(coerce *skel-op-types* 'list)))

(defstruct skel-op
  (type 0 :type unsigned-byte :read-only t)
  (scope 0 :type (unsigned-byte 64) :read-only t)
  (thunk #'identity :type function :read-only t))

(defstruct skel-vm
  (ip 0 :type (integer 0 #.*skel-stack-size*)) ;; to be atomic type needs to be (unsigned-byte 64)
  (stack (make-array *skel-stack-size* :element-type 'skel-op)
   :type (vector skel-op)))

(defvar *skel-scope*
  (let ((scope (init-skel-scope)))
    (init-skel-function-scope scope #'funcall)
    (init-skel-value-scope scope nil t)
    scope))

(defmacro with-skel-scope ((&optional (scope *skel-scope*)) &body body)
  `(let ((*skel-scope* ,scope))
     ,@body))

(defmacro with-skel-vm ((vm-sym &optional (vm (make-skel-vm))
                                          (scope *skel-scope*)
                                          (arena *skel-arena*))
                        &body body)
  `(sb-vm:with-arena (,arena)
     (let ((*skel-scope* ,scope)
           (*skel-arena* ,arena)
           (,vm-sym ,vm))
       (prog1
           ,@body
         (log:info! (format nil "skel-vm alloc-info: ~A/~A~%  userdata: ~A"
                            (sb-vm:arena-bytes-used ,arena)
                            (sb-vm:arena-length ,arena)
                            (sb-vm:arena-userdata ,arena)))))))
