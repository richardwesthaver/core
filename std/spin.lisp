;;; spin.lisp --- Spinlocks

;; CAS-based spinlocks using sb-ext:cas and sb-ext:spin-loop-hint

;;; Commentary:

;; based on LPARALLEL.SPIN-QUEUE

;; ref: https://github.com/lmj/lparallel/blob/master/src/spin-queue/cas-spin-queue.lisp

;;; Code:
(in-package :std/spin)
(defconstant +dummy+ :dummy)
(defconstant +dead-end+ :dead-end)

(defun make-spin-lock () 
  "Allocate a fresh 'spin-lock' which is simply NIL."
  nil)

(defstruct (spin-queue (:constructor %make-spin-queue (head tail)))
  (head (error "no head") :type cons)
  (tail (error "no tail") :type cons))

(defun make-spin-queue ()
  (let ((dummy (cons +dummy+ nil)))
    (%make-spin-queue dummy dummy)))

(defun push-spin-queue (value queue) 
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
  (null (cdr (spin-queue-head queue))))

(defun try-each-elem (fun queue)
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
  (tagbody
   :retry
     (let ((count 0))
       (unless (try-each-elem 
                (lambda (elem)
                  (declare (ignore elem))
                  (incf count))
                queue)
         (go :retry))
       (return-from spin-queue-count count))))

(defun peek-spin-queue (queue)
  (loop 
    until (try-each-elem 
           (lambda (elem)
             (return-from peek-spin-queue (values elem t)))
           queue))
  (values nil nil))
