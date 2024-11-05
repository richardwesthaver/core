;;; skel/core/vm.lisp --- The Skel Virtual Machine

;;; Commentary:

;; We have this idea that SBCL Arenas may be able to act as a sort of 'caution
;; tape' in the heap while the VM is running, but the usefulness of it is TBD.

;; The *SKEL-SCOPE* is currently a SO-MAP/FIXNUM (lockless structure) with
;; keys being simple sequential IDs ('scope-id') and values being vectors.

;; - 0 :: values
;; - 1 :: functions
;; - 2 :: user

;; The remaining values will be filled with temporary scopes as required by
;; the vm execution plan.

;; The *SKEL-STACK*

;;; Code:
(in-package :skel/core/vm)

(eval-always
  (defvar *skel-arena-size* (ash 1 16))
  (defvar *skel-stack-size* 128)
  (defun new-skel-arena () (sb-vm:new-arena *skel-arena-size*)))

(defun get-so-scope (so id)
  (when-let ((found (sb-lockless:so-find so id)))
    (sb-lockless:so-data found)))

(defun set-so-scope (so id env)
  (sb-lockless:so-insert so id env))

(defsetf get-so-scope set-so-scope)

(defvar *skel-arena*)

(defvar *skel-ops* nil)

(defvar *skel-scope*
  (let ((scope (sb-lockless:make-so-map/fixnum)))
    (set-so-scope scope 0 *skel-ops*)
    (set-so-scope scope 1 nil)
    scope))

(defvar *skel-stack*)

(defstruct (skel-op (:constructor make-skel-op (scope function)))
  (scope nil :type list :read-only t)
  (function #'identity :type function :read-only t))

(declaim (inline %sk-call))
(defun %sk-call (op) (funcall (skel-op-function op)))

;; TODO 2024-08-28: do we need to store arity or can we get by without it
;; being stored here?
(defmacro define-skel-op (name scope lambda-list &body body)
  "Define a SKEL-OP with a NAME TYPE, SCOPE and BODY which is compiled and stored
as the function slot."
  `(progn
     (defun ,(symbolicate "%SK-" name) ,lambda-list
       (make-skel-op ,scope
                     (compile nil (lambda () ,@body))))
     (pushnew ',(symbolicate "%SK-" name) *skel-ops*)))

;; math
(define-skel-op nil 0 () nil)
(define-skel-op eval 1 (form) (eval form))
(define-skel-op push 0 (val) (vector-push val *skel-stack*))
(define-skel-op pop 0 (val) (vector-push val *skel-stack*))
(define-skel-op clear 0 (scope) (sb-lockless:so-delete *skel-scope* scope))

(defun make-skel-stack (&optional (size *skel-stack-size*))
  (make-array size :element-type 'skel-op))

(defstruct skel-vm
  (ip 0 :type (integer 0 #.*skel-stack-size*)) ;; to be atomic type needs to be (unsigned-byte 64)
  (stack (make-skel-stack) :type (vector skel-op)))

(defmacro with-skel-vm ((vm-sym &optional (vm (make-skel-vm))
                                          (scope *skel-scope*)
                                          (arena *skel-arena*))
                        &body body)
  "Top-level entry to the SKEL-VM. *SKEL-SCOPE* and *SKEL-ARENA* are bound for
the duration of BODY."
  `(sb-vm:with-arena (,arena)
     (let ((*skel-scope* ,scope)
           (*skel-arena* ,arena)
           (,vm-sym ,vm))
       (prog1
           ,@body
         (log:trace! (format nil "skel-vm alloc-info: ~A/~A~%  userdata: ~A"
                            (sb-vm:arena-bytes-used ,arena)
                            (sb-vm:arena-length ,arena)
                            (sb-vm:arena-userdata ,arena)))))))
