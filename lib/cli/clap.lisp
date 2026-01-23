;;; cli/clap.lisp --- CLAP Protocol

;; 

;;; Code:
(in-package :cli/clap)

;;; Variables
(defparameter *no-exit* nil
  "Indicate whether the WITH-CLI-HANDLERS form should exit on completion.")

(defparameter *no-debug* nil
  "Indicate whether the WITH-CLI-HANDLERS form should enable the debugger.")

(defvar *cli* nil
  "The current CLI object.
This symbol is bound in the body of the WITH-CLI macro.")

(defvar *cli-table* (make-hash-table :test 'equal)
  "A hash table containing a mapping of names to CLI objects.")

(defvar *default-cli-class* 'cli)

;;; Utils
(defun arg0 () (car sb-ext:*posix-argv*))
(defun args () (cdr sb-ext:*posix-argv*))

(definline long-opt-p (str)
  (declare (simple-string str))
  (and (> (length str) 2)
       (char= (aref str 0) (aref str 1) #\-)))

(definline short-opt-has-eq-p (str)
  "Return non-nil if STR is a short-opt which has an '=' somewhere,
indicating a key/val pair without whitespace."
  (declare (simple-string str))
  (when-let ((pos (position #\= str :test 'char=)))
    (cons (subseq str 1 pos) (subseq str (1+ pos)))))

(definline long-opt-has-eq-p (str)
  "Return non-nil if STR is a long-opt which has an '=' somewhere,
indicating a key/val pair without whitespace."
  (declare (simple-string str))
  (when-let ((pos (position #\= str :test 'char=)))
    (cons (subseq str 2 pos) (subseq str (1+ pos)))))

(definline short-opt-p (str)
  (declare (simple-string str))
  (and (char= (aref str 0) #\-)
       (= (length str) 2)
       (not (char= (aref str 1) #\-))))

(definline multi-short-opt-p (str)
  "Return non-nil if STR is a multi-short-opt - prefixed with a single '-' but
containing multiple characters."
  (declare (simple-string str))
  (and (char= (aref str 0) #\-)
       (not (char= (aref str 1) #\-))))

(definline opt-keyword-p (str)
  (declare (simple-string str))
  (char= (aref str 0) #\:))

(definline opt-string-prefix-eq (ch str)
  (char= ch (aref str 0)))

(defun schar0 (name)
  "Return the first char of symbol or string NAME."
  (schar (string name) 0))

(defmacro with-cli-handlers (&body body)
  "A wrapper which handles common cli errors that may occur during
evaluation of BODY."
  `(progn
     (if *no-debug*
         (sb-ext:disable-debugger)
         (sb-ext:enable-debugger))
     (unwind-protect
          (restart-case 
              (handler-case (progn ,@body)
                (sb-sys:interactive-interrupt (c)
                  (if *no-debug*
                      (sb-ext:exit :code 130)
                      c))
                (error (c)
                  (println c)
                  (sb-ext:exit :code 1)))
            (abort ()
              :report (lambda (s)
                        (write-string
                         "Skip to toplevel REPL."
                         s)
                        (log:debug! "CONTINUEing from pre-REPL RESTART-CASE")
                        (values)))
            (exit ()
              :report "Exit SBCL (calling #'EXIT, killing the process)."
              ;; :test (lambda (c) (declare (ignore c)) t)
              (log:debug! "falling through to EXIT from pre-REPL RESTART-CASE~&")
              (sb-ext:exit :code 1))))
     (sb-impl::flush-standard-output-streams)
     (unless *no-exit*
       (sb-ext:exit :code 0))
     ;; reset terminal state
     #+nil (.ris)))

;;; Protocol
(defgeneric print-version (self &optional stream)
  (:documentation "Print the version of SELF."))

;;; CLI Command
(defkernel cli-command (command) ()
  (:documentation "Class of COMMANDs which may be executed directly from the command line."))

(defmethod make-load-form ((self cli-command) &optional env)
  (make-load-form-saving-slots self :environment env))

(defmethod call :before ((self cli-command) args)
  (log:trace! "calling command: ~A~@[ with args ~A~]~%" self args))

(init :commands :name :cli :class 'cli-command)

;;; CLI Command Types
(define-command-type string (input &optional (prompt "Input: "))
  (princ prompt *query-io*)
  (string (read-arg input)))

(define-command-type ustring (input &optional (prompt "INPUT: "))
  (princ prompt *query-io*)
  (string-upcase (read-arg input)))

(define-command-type dstring (input &optional (prompt "input: "))
  (princ prompt *query-io*)
  (string-downcase (read-arg input)))

(define-command-type * (input &optional (prompt "Input: "))
  (princ prompt *query-io*)
  (read-arg input))

(define-command-type y/n (input &optional prompt)
  (let ((*query-io* input))
    (y-or-n-p prompt)))

(define-command-type yes-or-no (input &optional prompt)
  (let ((*query-io* input))
    (yes-or-no-p prompt)))

(define-command-type char (input &optional (prompt "Character: "))
  (princ prompt *query-io*)
  (read-char input))

(define-command-type num (input &optional (prompt "Number: "))
  (princ prompt *query-io*)
  (parse-number (read-arg input)))

(define-command-type password (input &optional (prompt "Password: "))
  (format *query-io* prompt)
  (force-output *query-io*)
  (without-echo
    (string (read-arg input))))

(define-command-type command (input &optional (prompt "Command: ") (commands *commands*))
  (format *query-io* prompt)
  (command (read-arg input) commands))

;;; CLI
(defcommand (:cli :help) (&optional (arg *cli*))
  "Print help and exit."
  (declare (interactive (* "Command: ")))
  (print-help (if (cli-p arg) arg (command arg))))

(defcommand (:cli :version) (&optional (arg *cli*))
  "Print version and exit." 
  (declare (interactive *))
  (print-version arg *standard-output*))

;; REVIEW 2026-01-16: should this be a struct containing a CLI-COMMAND? hmm..
(defstruct cli
  ;; name slot defaults to *package*, must be string
  (name (string-downcase (package-name *package*)) :type string :read-only t)
  (version "0.1.0" :read-only t)
  description
  (cd *default-pathname-defaults*)
  (hook (make-instance 'key-hook))
  (main (required-argument :main) :type function))

(defmethod version ((self cli)) (cli-version self))
(defmethod name ((self cli)) (cli-name self))
(defaccessor kernel ((self cli)) (cli-main self))

(defmacro define-cli (name main
                      &key (version "0.1.0") 
                           hook
                           (cd *default-pathname-defaults*)
                           description)
  "Define a new CLI object.

NAME is assigned to the CLI and assumed to be the default binary name which
uses this object.

VERSION, DESCRIPTION, HOOK, CD, and MAIN are assigned to the associated slot
value of the CLI."
  `(let* ((*cli* (make-cli :name ',(string-downcase name) :version ,version :hook ,hook :cd ,cd :description ,description
                           :main (symbol-function ',main))))
     (load-cli *cli* ,name)
     *cli*))

(deffmt fmt-cli-usage "usage: ~A [opts] <command> [<arg>]~%" 
  "Given a NAME, print a basic cli usage string.")

(defmethod print-usage ((self cli) &optional stream)
  (fmt-cli-usage stream (name self)))

(defmethod print-version ((self cli) &optional stream)
  (println (version self) stream))

(deffmt fmt-cli-header "~A v~A --- ~A~%" "Given a NAME VERSION and DESCRIPTION, print a basic cli header.")

(defmethod print-help :before ((self cli) &optional (stream t))
  (fmt-cli-header stream (name self) (version self) (cli-description self)) stream)

(defmethod print-help ((self cli) &optional stream)
  (print-usage self stream)
  (let ((k (kernel self)))
    (format stream "~A~%" 
            (if (kernelp k)
                (kernel-documentation k)
                (documentation k 'function)))))

(defmethod equiv :before ((a cli) (b cli))
  "Return T if A is the same cli object as B.

Currently this function is intended only for instances of the CLI
class and is used as a specialized EQL for DEFINE-CONSTANT."
  (with-slots (version name ) a
    (with-slots ((bv version) (bn name)) b
      (and (equiv version bv)
           (string= name bn)))))

;; TODO 2025-06-13: call-with-cli
;; (defwith

(defmacro with-cli ((cli &key slots run exit)  &body body)
  "Like with-slots for CLI objects.

- CLI is an instance of a CLI class.

- SLOTS is a list passed directly to WITH-SLOTS with CLI.

- RUN with a non-nil value will interactively call the CLI after evaluating
  BODY.

- EXIT with a non-nil value will exit the current process as the last hook
  after evaluating BODY.

CLI is bound to *CLI*. ARGS is a list of CLI args, defaults to *ARGS* at
runtime if nil."
  `(progn
     (let ((*cli* ,cli))
       (setf (cli-cd *cli*) *default-pathname-defaults*)
       (with-slots ,slots *cli*
         ,@body
         ,@(when run '((call *cli*)))
         ,@(when exit '((sb-ext:exit)))))))

(defmethod exec ((self cli))
  (with-cli (self)
    (exec (kernel self))))

;;; CLI Package Helpers
(defun cli (&optional (name (package-name *package*)))
  (gethash (string-downcase name) *cli-table*))
(defun (setf cli) (new &optional (name (package-name *package*)))
  (setf (gethash (string-downcase name) *cli-table*) new))

(deferror missing-cli (simple-error) ()
  (:default-initargs :format-control "Missing CLI method for ~A"))
(defun missing-cli (key)
  (error 'missing-cli :format-arguments (list key)))

;; these functions are used to populate a *CLI-TABLE* record.
(defun load-cli (cli &optional (name (package-name *package*)))
  (declare (cli cli))
  (setf (cli name) cli))
