;;; mux.lisp --- Multiplexer

;; Based on IOLib (iomux)

;;; Code:
(in-package :io/mux)

(defvar *multiplexers* nil
  "A list of all available multiplexers.")

(defvar *default-multiplexer* nil
  "The default multiplexer for the current machine.")

(defvar *multiplexer-order* nil
  "An ordered list of multiplexers to prioritize. Higher priority items come first.")

(defclass event-base ()
  ((mux :reader mux)
   (fds :initform (make-hash-table :test 'eql)
        :reader fds)
   (timers :initform (make-pqueue :key #'sb-impl::%timer-expire-time)
           :reader timers)
   (fd-timers :initform (make-pqueue :key #'sb-impl::%timer-expire-time)
              :reader fd-timers)
   (expired-events :initform nil
                   :accessor expired-events)
   (write-interval-threshold :initarg :write-interval-threshold
                             :accessor write-interval-threshold)
   (state :initform nil :accessor state)
   (exit-when-empty :initarg :exit-when-empty :accessor exit-when-empty-p))
  (:default-initargs 
   :mux *default-multiplexer*
   :write-interval-threshold 0.0d0
   :exit-when-empty nil))

(defun set-io-handler (base fd &rest args))
(defun event-dispatch (base &rest args))
