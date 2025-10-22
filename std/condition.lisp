;;; condition.lisp --- Conditions and other exception handlers

;;; Code:
(in-package :std/condition)

(defun sb-grovel-unknown-constant-condition-p (c)
  "Detect SB-GROVEL unknown-constant conditions on older versions of SBCL"
  (and (typep c 'sb-int:simple-style-warning)
       (stringp (simple-condition-format-control c))
       (string= "Couldn't grovel for " (subseq (simple-condition-format-control c) 0 20))))

;;; Vars
(defvar *error-message* "An error occured"
  "The default error message used in STD-ERROR conditions.")
(defvar *handlers* nil
  "A list of condition handlers - often useful in asynchronous contexts.")

(defvar *uninteresting-conditions*
   '(sb-c::simple-compiler-note
     "&OPTIONAL and &KEY found in the same lambda list: ~S"
     sb-kernel:lexical-environment-too-complex
     sb-kernel:undefined-alien-style-warning
     sb-grovel-unknown-constant-condition ; defined above.
     sb-ext:implicit-generic-function-warning ;; Controversial.
     sb-int:package-at-variance
     sb-kernel:uninteresting-redefinition
     ;; BEWARE: the below four are controversial to include here.
     sb-kernel:redefinition-with-defun
     sb-kernel:redefinition-with-defgeneric
     sb-kernel:redefinition-with-defmethod
     sb-kernel:redefinition-with-defmacro)
   "A suggested value to which to set or bind *uninteresting-conditions*.")

;;; Utils
(defmacro nyi! (&optional comment)
  `(prog1
       (error "Not Yet Implemented!")
     (when ',comment (print ',comment))))

(declaim (inline car-eql))
(defun car-eql (a cons)
  "Return T if the CAR of CONS is EQL to A."
  (eql a (car cons)))

;;; Standard Conditions
(define-condition std-error (error)
  ((message :initarg :message
            :initform *error-message*
            :reader error-message))
  (:documentation "Standard Error")
  (:report (lambda (condition stream)
             (format stream "~A" (error-message condition)))))

(defun std-error (&rest args)
  "Signal an error of type STD-ERROR."
  (cerror
   "Ignore and continue"
   'std-error
   :message (format nil "~A: ~A" *error-message* args)))

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

;;; Deferror
(defmacro deferror (name (&rest parent-types) (&rest slot-specs) &rest options)
  "Define an error condition."
  (let ((fun (member :auto options :test #'car-eql))
        (%ancestors (flatten (mapcar (lambda (x) 
                                       (mapcar 'sb-mop:class-name 
                                               (sb-mop:class-precedence-list (find-class x))))
                                     parent-types))))
    (when fun
      (setf options (remove (car fun) options))
      (setf fun (cadar fun)))
    `(prog1
         (define-condition ,name ,(or parent-types '(std-error)) ,slot-specs ,@options)
       (when ',fun
         (cond 
           ((or
             (member 'invalid-item ',%ancestors)
             (member 'invalid-argument ',%ancestors))
            (def-invalid-item-reporter ,name))
           ((or (member 'simple-error ',%ancestors)
                (member 'simple-condition ',%ancestors))
            (def-simple-error-reporter ,name))
           ((stringp ',fun)
            (define-error-reporter ,name ',fun))
           (t (define-error-reporter ,name)))))))

(defmacro define-error-reporter (err &optional (message *error-message*))
    `(defun ,err (&rest args)
       ,(format nil "Signal an error of type ~A with ARGS." err)
       (cerror
        "Ignore and continue"
        ',err
        :message (format nil "~A: ~A" ,message args))))

(defmacro def-simple-error-reporter (name)
  `(progn
     (defun ,name (fmt &rest args)
       ,(format nil "Signal an error of type ~A with FMT string and ARGS." name)
       (cerror
        "Ignore and continue"
        ',name
        :format-control fmt
        :format-arguments args))))

(defmacro def-invalid-item-reporter (name)
  `(defun ,name (item &optional reason)
     ,(format nil "Signal an error of type ~A." name)
     (apply 'cerror
            "Ignore and continue"
            ',name
            :item item
            (when reason (list :reason reason)))))

;;; Defwarning      
(defmacro defwarning (name (&rest parent-types) (&rest slot-specs) &rest options)
  "Define an warning condition."
  (let ((fun (member :auto options :test #'car-eql)))
    (when fun (setq options (remove (car fun) options)))
    `(prog1
         (define-condition ,name ,(or parent-types '(std-warning)) ,slot-specs ,@options)
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

;;; Conditions
(defun required-argument (&optional name)
  "Signals an error for a missing argument of NAME. Intended for
use as an initialization form for structure and class-slots, and
a default value for required keyword arguments."
  (error "Required argument ~@[~S ~]missing." name))

;;;; Simple
(define-condition simple-style-warning (simple-warning style-warning)
  ()
  (:documentation "Simple style warnings."))

(defun simple-style-warning (message &rest args)
  "Signal a SIMPLE-STYLE-WARNING using format-contorl MESSAGE and format-arguments ARGS."
  (warn 'simple-style-warning :format-control message :format-arguments args))

;; We don't specify a :report for simple-reader-error to let the
;; underlying implementation report the line and column position for
;; us. Unfortunately this way the message from simple-error is not
;; displayed, unless there's special support for that in the
;; implementation. But even then it's still inspectable from the
;; debugger...
(define-condition simple-reader-error (sb-int:simple-reader-error)
  ()
  (:documentation "Simple reader errors."))

(defun simple-reader-error (stream message &rest args)
  "Signal an error of type SIMPLE-READER-ERROR."
  (error 'simple-reader-error
         :stream stream
         :format-control message
         :format-arguments args))

(define-condition simple-parse-error (simple-error parse-error) ()
  (:documentation "Simple parse errors."))

(defun simple-parse-error (message &rest args)
  "Signal an error of type SIMPLE-PARSE-ERROR."
  (error 'simple-parse-error
         :format-control message
         :format-arguments args))

(define-condition simple-program-error (simple-error program-error)
  ()
  (:documentation "Simple program errors."))

(defun simple-program-error (message &rest args)
  "Signal an error of type SIMPLE-PROGRAM-ERROR."
  (error 'simple-program-error
         :format-control message
         :format-arguments args))

(define-condition circular-dependencies (simple-error)
  ((items
    :initarg :items
    :initform (error "Must specify items")
    :reader error-items))
  (:report (lambda (c s) (format s "Circular dependency detected in list ~a" (error-items c))))
  (:documentation "A condition which is signalled when a circular dependency is encountered."))

(define-condition unknown-argument (error)
  ((name
    :initarg :name
    :initform (error "Must specify argument name")
    :reader error-name)
   (type
    :initarg :type
    :initform (error "Must specify argument type")
    :reader error-type))
  (:report (lambda (c s) (format s "Unknown argument ~A of type ~A" (error-name c) (error-type c))))
  (:documentation "A condition which is signalled when an unknown argument is encountered."))

(defun unknown-argument-p (value)
  "Return T if VALUE is a condition of type UNKNOWN-ARGUMENT."
  (typep value 'unknown-argument))

(define-condition missing-argument (simple-error)
  ((item
    :initarg :item
    :initform (error "Must specify argument item")
    :reader error-item))
   (:report (lambda (c s) (format s "Missing argument ~a." (error-item c))))
   (:documentation "A condition which is signalled when an option expects an argument, but none
was provided."))

(defun missing-argument-p (value)
  "Return T if VALUE is a condition of type MISSING-ARGUMENT."
  (typep value 'missing-argument))

(define-condition invalid-item ()
  ((item
    :initarg :item
    :initform (error "Must specify argument item")
    :reader error-item
    :documentation "The item which is identified as invalid")
   (reason
    :initarg :reason
    :initform (error "Must specify reason")
    :reader error-reason
    :documentation "The reason why this item is invalid"))
  (:report (lambda (c s) (format s "Invalid item: ~A~%Reason: ~A" (error-item c) (error-reason c))))
  (:documentation "A condition which is signalled when an argument is identified as invalid."))

(define-condition invalid-argument (simple-error invalid-item) ()
  (:report (lambda (c s) (format s "Invalid argument: ~A~%Reason: ~A" (error-item c) (error-reason c))))             
  (:documentation "Invalid argument errors."))

(define-condition conflicting-arguments (simple-error invalid-item) ()
  (:report (lambda (c s)
             (format s "Conflicting arguments: ~A~%Reason: ~A" (error-item c) (error-reason c))))
  (:documentation "Conflicting argument errors."))

(define-condition unknown-token (std-error)
  ((token :reader error-item :initarg :token))
  (:report (lambda (c s) (format s "Unknown token: ~a" (error-item c))))
  (:documentation "Unknown token errors."))

(defmethod print-object ((c unknown-token) stream)
  (when (slot-boundp c 'token)
    (format stream "Unknown token: ~A~%" (error-item c)))
  (call-next-method))

(defun interact (&rest prompt)
  "Read from user and eval."
  (apply #'format *query-io* prompt)
  (finish-output *query-io*)
  (multiple-value-list (eval (read *query-io*))))

(defun interact* (&rest prompt)
  "Read from user."
  (apply #'format *query-io* prompt)
  (finish-output *query-io*)
  (read *query-io*))

(defun interact-line (&rest prompt)
  "Read a line from user, return a string."
  (apply #'format *query-io* prompt)
  (finish-output *query-io*)
  (read-line *query-io*))

;;; Macros
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

;;; Debugger
;; from hunchentoot
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

;;; Meta
(define-condition meta-condition () ()
  (:documentation "A condition which is signalled somewhere within the CLOS/MOP machinery."))

(define-condition missing-method (error meta-condition)
  ((method :initarg :method :reader error-item))
  (:report (lambda (c s) (format s "Missing method ~a." (error-item c))))
  (:documentation "Missing CLOS method errors."))

(define-condition missing-methods (error meta-condition)
  ((methods :initarg :methods :reader error-items))
  (:report (lambda (c s) (format s "The following methods are missing: ~{~a~^, ~}" (error-items c))))
  (:documentation "Multiple missing CLOS methods errors."))

;;;; Wrapped
(define-condition wrapped-condition ()
  ((value :type condition :reader wrapped-condition-value :initarg :value))
  (:documentation 
   "A container for transporting conditions - usually to another thread."))

(defun wrap-condition (condition)
  "Wrap a condition. A non-error condition may also be wrapped, though it
will still be signaled with `signal'."
  (make-condition 
   'wrapped-condition
   :value (ctypecase condition
	    (symbol (make-condition condition))
	    (condition condition))))

(define-condition wrapped-error (wrapped-condition) ()
  (:documentation "A container for transporting errors - usually to another thread."))

(defun wrap-error (condition)
  "Wrap an error. A non-error condition may also be wrapped, though it
will still be signaled with `error'."
  (make-condition 
   'wrapped-error
   :value (ctypecase condition
            (symbol (make-condition condition))
            (condition condition))))
