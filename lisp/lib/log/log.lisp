(in-package :log)

(deftype log-level-designator () '(member :warn :debug :info :trace))
(declaim (type (or boolean log-level-designator) *log-level*))
(defparameter *log-level* nil)
(defparameter *logger* nil)
(defparameter *log-router* nil)
(declaim (type (or boolean function) *log-timestamp*))
(defparameter *log-timestamp* t 
  "If non-nil, print a timestamp with log output. The value may be a
function in which case it is used as the function value of
`log-timestamp-source'.")

;; TODO 2023-09-20: (declaim (inline log-timestamp-source)) ;; this
;; probably shouldn't be inlined.. bench it
(defun log-timestamp-source ()
  (if (functionp *log-timestamp*)
      (funcall *log-timestamp*)
      (format nil "~f" (/ (get-internal-real-time) internal-time-units-per-second))))

;; the purpose of this struct is to route log messages to the
;; appropriate output stream. It should be configured and bound to
;; *LOG-ROUTER*.
(defstruct log-router
  info error debug trace)

;; TODO 2023-09-20: make-synonym-stream, make-synonym-stream-symbol 
(defvar *default-log-router* 
  (make-log-router :info *terminal-io* 
		   :error *error-output* 
		   :debug *debug-io*
		   :trace *trace-output*))

(defstruct logger
  (active nil :type boolean)
  (timestamp *log-timestamp* :type (or boolean function))
  (router *default-log-router* :type log-router))
   
(defmacro info! (opts &rest args))

(defmacro trace! (opts &rest args))

(defmacro warn! (opts &rest args))

(defun debug-p () (eq *log-level* :debug))

(defun debug-log-line ()
  (format t ":DEBUG~A~%"
	  (if *log-timestamp*
	      (format nil " @ ~A ~t" (log-timestamp-source))
	      "")))

;; TODO 2023-08-31: single format control string
(defun debug! (&rest args)
  (when (debug-p)
    (debug-log-line)
    ;; RESEARCH 2023-08-31: what's better here.. loop, do, mapc+nil?
    (map nil (lambda (x) (format t "; ~X~%" x)) args))
  args)

(defun dbg! (&rest args)
  (when (debug-p)
    (debug-log-line)
    (apply #'describe args)))
