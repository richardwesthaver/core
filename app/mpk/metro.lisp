;;; metro.lisp --- MIDI/OSC Client/Server

;; 

;;; Code:
(in-package :mpk/metro)

(defvar *metro* nil)
(defvar *metro-table* (make-hash-table))

(defkernel metro (scheduled-task)
  ((name :initarg :name :reader name)
   (clock :initarg :clock :reader metro-clock))
  (:documentation "A metronome-like object which is designed to be called in a dedicated worker
thread. This object is similar to SB-EXT:TIMER but much less dynamic - you
should only use this to run high-precision clock functions with minimal
latency. The KERNEL slot of a METRO instance is currently assumed to return
two values - a boolean and a positive integer representing the tick count.
The first value is T on a downbeat and may be NIL on upbeats.")
  (:kernel (lambda (self) (values t (incf self)))))

(defmethod print-object ((self metro) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~@[~A~] :function ~A" (name self) (kernel self))))

(defun init-metro (name &optional (register t))
  (lety ((m (make-instance 'metro :name name) :type metro))
    (setq *metro* m)
    (when register
      (setf (gethash name *metro-table*) m))))
