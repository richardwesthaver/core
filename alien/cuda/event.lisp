;;; event.lisp --- CUDA Events

;; 

;;; Code:
(in-package :cuda)

(defun create-cu-event ()
  (with-alien ((e cu-event))
    (cu-event-create (addr e) #.(cu-event-flag :default))
    e))

(defun record-cu-event (cu-event)
  (cu-event-record cu-event nil))

(defun %elapsed-time (start stop)
  (with-alien ((msec float))
    (cu-event-elapsed-time (addr msec) start stop)
    msec))

(defstruct cuda-timer
  (start nil :read-only t)
  (stop nil :read-only t))

(defun create-cuda-timer ()
  (make-cuda-timer :start (create-cu-event) :stop (create-cu-event)))

(defmethod free ((self cuda-timer))
  (cu-event-destroy (cuda-timer-start self))
  (cu-event-destroy (cuda-timer-stop self)))

(defmethod start ((self cuda-timer))
  (record-cu-event (cuda-timer-start self)))

(defmethod stop ((self cuda-timer) &key)
  (record-cu-event (cuda-timer-stop self)))

(defmethod sync ((self cuda-timer) &key)
  (cu-event-synchronize (cuda-timer-stop self)))

(defun elapsed-time (self)
  (declare (cuda-timer self))
  (%elapsed-time (cuda-timer-start self) (cuda-timer-stop self)))

(defmacro with-cuda-timer (var &body body)
  `(let ((,var (create-cuda-timer)))
     (declare (cuda-timer ,var))
     (unwind-protect (progn ,@body)
       (free ,var))))
