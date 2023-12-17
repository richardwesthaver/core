;;; VM

;; Stack slots refer to objects. a Stack is a sequence of objects
;; which can be output to a stream using a specialized function.

;;; Code:
(in-package :skel)
(deftype stack-slot-kind () '(member :shell :lisp :comment :var :rule :directive :nop))

(defstruct stack-slot 
  (kind :nop :type stack-slot-kind) 
  (spec nil :type sxp:form) 
  (form nil :type sxp:form))
  
(declaim (inline %make-sk-vm))
(defstruct (sk-vm (:constructor %make-sk-vm))
  ;; TODO 2023-09-23: consider making this an open closure, call it in
  ;; MAKE-SK-VM.
  (ip (make-stack-slot) :type stack-slot)
  (stack (make-array 0) :type (array stack-slot)))

(defun make-sk-vm (size) 
  (let ((vm (%make-sk-vm :stack (make-array size :fill-pointer t :initial-element (make-stack-slot)))))
    (with-slots (ip stack) vm
      (setf ip (aref stack 0))
    vm)))

(defmethod sks-ref ((vm sk-vm)) (setf (sk-vm-ip vm) (aref (sk-vm-stack vm) 0)))

(defmethod sks-pop ((vm sk-vm)) (setf (sk-vm-ip vm) (vector-pop (sk-vm-stack vm))))

(defmethod sks-push ((slot stack-slot) (vm sk-vm)) (vector-push slot (sk-vm-stack vm)))
