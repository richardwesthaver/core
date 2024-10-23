;;; condition.lisp --- Conditions and other exception handlers

;;; Code:
(in-package :std/condition)

(defvar *std-error-message* "An error occured")

(define-condition std-error (error)
  ((message :initarg :message
            :initform *std-error-message*
            :reader std-error-message))
  (:documentation "Standard Error")
  (:report (lambda (condition stream)
             (format stream "~A" (std-error-message condition)))))

(defun std-error (&rest args)
  (cerror
   "Ignore and continue"
   'std-error
   :message (format nil "~A: ~A" *std-error-message* args)))

(define-condition std-warning (warning)
  ((message :initarg :message
            :initform nil
            :reader std-warning-message))
  (:documentation "Standard Warning")
  (:report
   (lambda (condition stream)
     (when (std-warning-message condition)
       (format stream "~X" (std-warning-message condition))))))

(defun std-warning (&optional message)
  (warn 'std-warning :message message))
  
(defun car-eql (a cons)
  (eql a (car cons)))

(defmacro deferror (name (&rest parent-types) (&rest slot-specs) &rest options)
  "Define an error condition."
  (let ((fun (member :auto options :test #'car-eql)))
    (when fun (setq options (remove (car fun) options)))
    `(prog1
         (define-condition ,name ,(or parent-types '(std-error)) ,slot-specs ,@options)
       (when ',fun
         (if (or (member 'simple-error ',parent-types)
                 (member 'simple-condition ',parent-types))
             (def-simple-error-reporter ,name)
             (def-error-reporter ,name))))))

(defmacro def-error-reporter (err)
    `(defun ,err (&rest args)
       ,(format nil "Signal an error of type ~A with ARGS." err)
       (cerror
        "Ignore and continue"
        ',err
        :message (format nil "~A: ~A" ,*std-error-message* args))))

(defmacro def-simple-error-reporter (name)
  `(progn
     (defun ,name (fmt &rest args)
       ,(format nil "Signal an error of type ~A with FMT string and ARGS." name)
       (cerror
        "Ignore and continue"
        ',name
        :format-control fmt
        :format-arguments args))))

(defmacro defwarning (name (&rest parent-types) (&rest slot-specs) &rest options)
  "Define an warning condition."
  (let ((fun (member :auto options :test #'car-eql)))
    (when fun (setq options (remove (car fun) options)))
    `(prog1
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (define-condition ,name ,(or parent-types '(std-warning)) ,slot-specs ,@options))
       (when ',fun
         (if (or (find 'simple-warning ',parent-types)
                 (find 'simple-condition ',parent-types))
             (def-simple-warning-reporter ,name)
             (def-warning-reporter ,name))))))

(defmacro def-warning-reporter (name)
  `(defun ,name (&optional message)
       ,(format nil "Signal a warning of type ~A with optional MESSAGE." name)
       (warn
        ',name
        :message message)))

(defmacro def-simple-warning-reporter (name)
  `(defun ,name (fmt &rest args)
     ,(format nil "Signal an error of type ~A with FMT string and ARGS." name)
     (warn
      ',name
      :format-control fmt
      :format-arguments args)))

(defmacro nyi! (&optional comment)
  `(prog1
       (error "Not Yet Implemented!")
     (when ',comment (print ',comment))))

(defun required-argument (&optional name)
  "Signals an error for a missing argument of NAME. Intended for
use as an initialization form for structure and class-slots, and
a default value for required keyword arguments."
  (error "Required argument ~@[~S ~]missing." name))

(define-condition simple-style-warning (simple-warning style-warning)
  ())

(defun simple-style-warning (message &rest args)
  (warn 'simple-style-warning :format-control message :format-arguments args))

;; We don't specify a :report for simple-reader-error to let the
;; underlying implementation report the line and column position for
;; us. Unfortunately this way the message from simple-error is not
;; displayed, unless there's special support for that in the
;; implementation. But even then it's still inspectable from the
;; debugger...
(define-condition simple-reader-error
    (sb-int:simple-reader-error)
  ())

(defun simple-reader-error (stream message &rest args)
  (error 'simple-reader-error
         :stream stream
         :format-control message
         :format-arguments args))

(define-condition simple-parse-error (simple-error parse-error)
  ())

(defun simple-parse-error (message &rest args)
  (error 'simple-parse-error
         :format-control message
         :format-arguments args))

(define-condition simple-program-error (simple-error program-error)
  ())

(defun simple-program-error (message &rest args)
  (error 'simple-program-error
         :format-control message
         :format-arguments args))

(define-condition circular-dependency (simple-error)
  ((items
    :initarg :items
    :initform (error "Must specify items")
    :reader circular-dependency-items))
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Circular dependency detected")))
  (:documentation "A condition which is signalled when a circular dependency is encountered."))

(define-condition unknown-argument (error)
  ((name
    :initarg :name
    :initform (error "Must specify argument name")
    :reader unknown-argument-name)
   (kind
    :initarg :kind
    :initform (error "Must specify argument kind")
    :reader unknown-argument-kind))
  (:report (lambda (condition stream)
             (format stream "Unknown argument ~A of kind ~A"
                     (unknown-argument-name condition)
                     (unknown-argument-kind condition))))
  (:documentation "A condition which is signalled when an unknown argument is encountered."))

(defun unknown-argument-p (value)
  (typep value 'unknown-argument))

(define-condition missing-argument (simple-error)
  ((item
    :initarg :item
    :initform (error "Must specify argument item")
    :reader missing-argument-item))
   (:report (lambda (condition stream)
              (declare (ignore condition))
              (format stream "Missing argument")))
   (:documentation "A condition which is signalled when an option expects an argument, but none was provided"))

(defun missing-argument-p (value)
  (typep value 'missing-argument))

(define-condition invalid-argument (simple-error)
  ((item
    :initarg :item
    :initform (error "Must specify argument item")
    :reader invalid-argument-item
    :documentation "The argument which is identified as invalid")
   (reason
    :initarg :reason
    :initform (error "Must specify reason")
    :reader invalid-argument-reason
    :documentation "The reason why this argument is invalid"))
  (:report (lambda (condition stream)
             (format stream "Invalid argument: ~A~%Reason: ~A" (invalid-argument-item condition) (invalid-argument-reason condition))))
  (:documentation "A condition which is signalled when an argument is identified as invalid."))

(defmacro ignore-some-conditions ((&rest conditions) &body body)
  "Similar to CL:IGNORE-ERRORS but the (unevaluated) CONDITIONS
list determines which specific conditions are to be ignored."
  `(handler-case
       (progn ,@body)
     ,@(loop for condition in conditions collect
             `(,condition (c) (values nil c)))))

(defmacro unwind-protect-case ((&optional abort-flag) protected-form &body clauses)
  "Like CL:UNWIND-PROTECT, but you can specify the circumstances that
the cleanup CLAUSES are run.

  clauses ::= (:NORMAL form*)* | (:ABORT form*)* | (:ALWAYS form*)*

Clauses can be given in any order, and more than one clause can be
given for each circumstance. The clauses whose denoted circumstance
occured, are executed in the order the clauses appear.

ABORT-FLAG is the name of a variable that will be bound to T in
CLAUSES if the PROTECTED-FORM aborted preemptively, and to NIL
otherwise.

Examples:

  (unwind-protect-case ()
       (protected-form)
     (:normal (format t \"This is only evaluated if PROTECTED-FORM executed normally.~%\"))
     (:abort  (format t \"This is only evaluated if PROTECTED-FORM aborted preemptively.~%\"))
     (:always (format t \"This is evaluated in either case.~%\")))

  (unwind-protect-case (aborted-p)
       (protected-form)
     (:always (perform-cleanup-if aborted-p)))
"
  (check-type abort-flag (or null symbol))
  (let ((gflag (gensym "FLAG+")))
    `(let ((,gflag t))
       (unwind-protect (multiple-value-prog1 ,protected-form (setf ,gflag nil))
	 (let ,(and abort-flag `((,abort-flag ,gflag)))
	   ,@(loop for (cleanup-kind . forms) in clauses
		   collect (ecase cleanup-kind
			     (:normal `(when (not ,gflag) ,@forms))
			     (:abort  `(when ,gflag ,@forms))
			     (:always `(progn ,@forms)))))))))

;; hunchentoot
(defvar *catch-errors-p* nil
  "When non-nil catch and log errors instead of invoking the debugger.")

(defgeneric maybe-invoke-debugger (condition)
  (:documentation "This generic function is called whenever a
condition CONDITION is signaled in Hunchentoot.  You might want to
specialize it on specific condition classes for debugging purposes.")
  (:method (condition)
   "The default method invokes the debugger with CONDITION if
*CATCH-ERRORS-P* is NIL."
   (unless *catch-errors-p*
     (invoke-debugger condition))))

(defmacro with-debugger (&body body)
  "Executes BODY and invokes the debugger if an error is signaled and
*CATCH-ERRORS-P* is NIL."
  `(handler-bind ((error #'maybe-invoke-debugger))
     ,@body))

(defmacro ignore-errors* (&body body)
  "Like IGNORE-ERRORS, but observes *CATCH-ERRORS-P*."
  `(ignore-errors (with-debugger ,@body)))

(defmacro handler-case* (expression &rest clauses)
  "Like HANDLER-CASE, but observes *CATCH-ERRORS-P*."
  `(handler-case (with-debugger ,expression)
     ,@clauses))

(defun get-backtrace ()
  "Returns a string with a backtrace of what the Lisp system thinks is
the 'current' error."
  (handler-case
      (with-output-to-string (s)
        (sb-debug:print-backtrace :stream s))
    (error (condition)
      (format nil "Could not generate backtrace: ~A." condition))))
