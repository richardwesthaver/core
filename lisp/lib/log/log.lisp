(in-package :log)

(deftype log-level-designator () '(member nil :fatal :error :warn :info :debug :trace t))

(declaim (log-level-designator *log-level*))
(defparameter *log-level* :debug
  "Logging is performed dynamically based on this variable. When NIL,
logging is disabled, which is equivalent to a level of :FATAL. When T,
Logging is enabled for all levels, which is equivalent to :TRACE.")

(defvar *logger* nil)

(defvar *log-router* nil)

(defvar *log-timestamp* t 
  "If non-nil, print a timestamp with log output. The value may be a
function in which case it is used as the function value of
`log-timestamp-source', or a number which will be used as the input arg to GET-REAL-TIME-SINCE.")

(declaim (fixnum *log-indent*))
(defvar *log-indent* 0
  "Level of indentation to apply to multi-line log messages.")

(defun get-real-time-since (n)
  "Return the numbers of seconds since a relative value offset N."
  (- (get-internal-real-time) n))

(defun init-log-timestamp ()
  (setq *log-timestamp* (get-internal-real-time)))

;; TODO 2023-09-20: (declaim (inline log-timestamp-source)) ;; this
;; probably shouldn't be inlined.. bench it
(defun log-timestamp-source ()
  (typecase *log-timestamp*
    (function (funcall *log-timestamp*))
    (number (/ (get-real-time-since *log-timestamp*) #.internal-time-units-per-second))
    (t (/ (get-internal-real-time) #.internal-time-units-per-second))))

(defun universal-timestamp () (get-universal-time))
  
;; the purpose of this struct is to route log messages to the
;; appropriate output stream. It should be configured and bound to
;; *LOG-ROUTER*.
(defstruct log-router
  fatal error warn info debug trace)

;; TODO 2023-09-20: make-synonym-stream, make-synonym-stream-symbol 
(defvar *default-log-router* 
  (make-log-router
   :fatal *error-output*
   :error *error-output*
   :warn *debug-io*
   :info *terminal-io* 
   :debug *debug-io*
   :trace *trace-output*))

(defstruct logger
  "The logger is responsible for intercepting log messages and either
printing them to a stream based on the router slot, or doing nothing
based on the level slot. Additionally, the appenders slot may contain
a list of functions taking a single log message as input. Each
appender in the list is called on each message intercepted wrt level."
  (level *log-level* :type log-level-designator)
  (timestamp *log-timestamp* :type (or boolean function number))
  (appenders nil :type list)
  (router *default-log-router* :type log-router))

;; TODO: (defmacro generate-log-profile)
;; (defmacro deflogger) ;; yalog
;; (defmacro with-log-profile)
(defmacro with-logger ((logger) &body body)
  "Activate the specified logger for the life-time of BODY. This is
useful if you don't want to dynamically overwrite the *LOGGER*
binding."
  `(let ((*logger* ,logger))
     ,@body))

(defmacro define-log-level (name &body pred)
  "Define a log-level of NAME with PRED being the body of the predicate
function 'NAME-P'."
  (let ((%name (string-upcase name)))
    `(progn
       (defun ,(intern (concatenate 'string %name "-P")) ()
         ,@(or pred `((eql *log-level* ,(sb-int:keywordicate name)))))
       (defun ,(intern (concatenate 'string %name "!")) (&rest args)
         (when (,(symbolicate (concatenate 'string %name "-P")))
         (format t "#:~(~A~) ~@[~f~]"
                 ',name
                 (when *log-timestamp* (log-timestamp-source)))
         (mapc (lambda (x) (format t "; ~A~%" x)) args))
         (if (= 1 (length args))
             (car args)
             args))
       (defun ,(intern (concatenate 'string %name "-DESCRIBE")) (&rest args)
         (,(intern (concatenate 'string %name "!")) (apply #'describe args))))))

(define-log-level trace (or (eql *log-level* :trace) (eql *log-level* t)))
(define-log-level debug (or (trace-p) (eql *log-level* :debug)))
(define-log-level info (or (debug-p) (eql *log-level* :info)))
(define-log-level warn (or (info-p) (eql *log-level* :warn)))
(define-log-level error (or (warn-p) (eql *log-level* :error)))
(define-log-level fatal t) ;; probably needs to be a special case

;; TODO 2023-08-31: single format control string
;; (defun debug! (&rest args)
;;   (when (debug-p)
;;     ;...
;;     ;; RESEARCH 2023-08-31: what's better here.. loop, do, mapc+nil?
;;     (map nil (lambda (x) (format t "~X~%" x)) args))
;;   args)
