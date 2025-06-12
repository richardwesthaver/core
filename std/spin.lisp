;;; spin.lisp --- Spinlocks

;; CAS-based spinlocks using sb-ext:cas and sb-ext:spin-loop-hint

;;; Commentary:

;; based on LPARALLEL.SPIN-QUEUE

;; ref: https://github.com/lmj/lparallel/blob/master/src/spin-queue/cas-spin-queue.lisp

;;; Code:
(in-package :std/spin)
(declaim (optimize (speed 3)))

(defconstant +dummy+ :dummy
  "Dummy SPIN-QUEUE value.")

(defconstant +dead-end+ :dead-end
  "Dead-end value for SPIN-QUEUEs.")

(defun make-spin-lock () 
  "Allocate a fresh 'spin-lock' which is simply NIL."
  nil)

(defstruct (spin-queue (:constructor %make-spin-queue (head tail)))
  "CAS-based spin-lock queue."
  (head (error "no head") :type cons)
  (tail (error "no tail") :type cons))

(defun make-spin-queue ()
  "Make a fresh SPIN-QUEUE."
  (let ((dummy (cons +dummy+ nil)))
    (%make-spin-queue dummy dummy)))

(defun push-spin-queue (value queue) 
  "Push VALUE onto QUEUE."
  (declare (ftype (function (t spin-queue) (values)) push-spin-queue))
  ;; Attempt CAS, repeat upon failure. Upon success update QUEUE-TAIL.
  (let ((new (cons value nil)))
    (loop (when (cas (cdr (spin-queue-tail queue)) nil new)
            (setf (spin-queue-tail queue) new)
            (return (values))))))

(defun pop-spin-queue (queue) 
  "Attempt to CAS QUEUE-HEAD with the next node, repeat upon failure. Upon
success, clear the discarded node and set the CAR of QUEUE-HEAD to +DUMMY+."
  (declare (ftype (function (spin-queue) (values t boolean))))
  (loop (let* ((head (spin-queue-head queue))
               (next (cdr head)))
          ;; NEXT could be +DEAD-END+, whereupon we try again.
          (typecase next
            (null (return (values nil nil)))
            (cons (when (cas (spin-queue-head queue) head next)
                    (let ((value (car next)))
                      (setf (cdr head) +dead-end+
                            (car next) +dummy+)
                      (return (values value t)))))))))

(defun spin-queue-empty-p (queue)
  "Return T if QUEUE is empty."
  (null (cdr (spin-queue-head queue))))

(defun try-each-elem (fun queue)
  "Try FUN on each element of QUEUE."
  (declare ((function (spin-queue) (values t boolean)) fun))
  (let ((node (spin-queue-head queue)))
    (loop
      (let ((value (car node)))
        (unless (eq value +dummy+)
          (funcall fun value)))
      (setf node (cdr node))
      (cond 
        ((eq node +dead-end+)
         (return nil))
        ((null node)
         (return t))))))

(defun spin-queue-count (queue)
  "Return the count of QUEUE."
  (tagbody
   :retry
     (let ((count 0))
       (declare (fixnum count))
       (unless (try-each-elem
                (lambda (elem)
                  (declare (ignore elem))
                  (incf count))
                queue)
         (go :retry))
       (return-from spin-queue-count count))))

(defun peek-spin-queue (queue)
  "Peek at the next element of QUEUE."
  (declare (optimize (safety 0)))
  (loop 
    until (try-each-elem 
           (lambda (elem)
             (return-from peek-spin-queue (values elem t)))
           queue))
  (values nil nil))
