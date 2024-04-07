(in-package :log)

(deftype log-level-designator () '(member :warn :debug :info :trace))
(declaim (type (or boolean log-level-designator) *log-level*))
(defvar *log-level* nil)
(defvar *logger* nil)
(defvar *log-router* nil)
(declaim (type (or boolean function number) *log-timestamp*))
(defvar *log-timestamp* t 
  "If non-nil, print a timestamp with log output. The value may be a
function in which case it is used as the function value of
`log-timestamp-source'.")

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
    (number (format nil "~f" (/ (get-real-time-since *log-timestamp*) #.internal-time-units-per-second)))
    (t (format nil "~f" (/ (get-internal-real-time) #.internal-time-units-per-second)))))

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

;; TODO: (defmacro generate-log-profile)
;; (defmacro deflogger) ;; yalog
;; (defmacro with-log-profile)
;; (defmacro with-logger)
(defmacro define-log-level (name)
  (let ((%name (string-upcase name)))
    `(progn
       (defun ,(intern (concatenate 'string %name "!")) (&rest args)
         (format t "#:~(~A~) ~A~%"
                 ',name
                 (if *log-timestamp*
                     (log-timestamp-source)
                     ""))
         (mapc (lambda (x) (format t "~t; ~X~%" x)) args)
         (if (= 1 (length args))
             (car args)
             args))
       (defun ,(intern (concatenate 'string %name "-P")) ()
         (eql *log-level* ,(sb-int:keywordicate name)))
       (defun ,(intern (concatenate 'string %name "-DESCRIBE")) (&rest args)
         (,(intern (concatenate 'string %name "!")) (apply #'describe args))))))

(define-log-level info)
(define-log-level trace)
(define-log-level warn)
(define-log-level debug)

#+nil (test! "foo")

;; (defmacro info! (opts &rest args))

;; (defmacro trace! (opts &rest args))

;; (defmacro warn! (opts &rest args))

;; (defun debug-p ()
;;   (or (eq *log-level* t)
;;       (eq *log-level* :debug)))

;; TODO 2023-08-31: single format control string
;; (defun debug! (&rest args)
;;   (when (debug-p)
;;     ;...
;;     ;; RESEARCH 2023-08-31: what's better here.. loop, do, mapc+nil?
;;     (map nil (lambda (x) (format t "~X~%" x)) args))
;;   args)

;; (defun debug-describe (&rest args)
;;   (debug! (apply #'describe args)))

