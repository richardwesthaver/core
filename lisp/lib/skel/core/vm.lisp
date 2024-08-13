;;; skel/core/vm.lisp --- The Skel Virtual Machine

;; Stack slots refer to objects. a Stack is a sequence of objects
;; which can be output to a stream using a specialized function.

;;; Code:
(in-package :skel/core/vm)

(defvar *skel-op-types*
  (list :nop :eval :set :get :end :jump :pop :spawn :wait :print :let))

(deftype skel-op-type () `(member ,@*skel-op-types*))

(defstruct skel-op
  (type :nop :type skel-op-type)
  body)
  
(defstruct skel-vm
  (ip (make-stack-slot) :type stack-slot)
  (stack (make-array 0) :type (array stack-slot)))

(defvar *skel-arena-size* (ash 1 16))
(defvar *skel-arenas* nil)

(defun new-skel-arena () (sb-vm:new-arena *skel-arena-size*))

(sb-ext:defglobal *skel-arena* (make-skel-arena))

;; (defmacro with-skel-arena (arena &body body))
;; (defmacro with-skel-stack ((stack &key arena) &body body))
;; (defmacro with-skel-vm ((vm &optional arena) &body body))
