;;; cli/clap.lisp --- CLAP Protocol

;; 

;;; Commentary:

;; =clap= is a name borrowed from the [[https://github.com/clap-rs/clap][clap.rs]] Command Line Argument
;; Parser for Rust. What can I say, it's a good acronym.

;; Besides the system tests, you can find this package used in our lisp
;; binaries (=bin=).

;; =clap= provides an object protocol and classes which parse input into
;; a simple AST and execute the appropriate option and command handlers.

;; Several macros form the public API used to build a CLI including
;; =defcommand=, =defopt=, =define-cli=, and =defmain=.

;;; Code:
(in-package :cli/clap)
(init :commands :name :cli :class 'cli-command :clean t)

;;; Variables
(defvar *cli* nil
  "The current CLI object.
This symbol is bound in the body of the WITH-CLI macro.")

(defvar *cli-table* (make-hash-table :test 'equal)
  "A hash table containing a mapping of names to CLI objects.")

(defvar *default-cli-class* 'cli)

;;; Utils
(defun arg0 () (car sb-ext:*posix-argv*))
(defun args () (cdr sb-ext:*posix-argv*))

(defun cli-args () 
  "Like CLI:ARGS but intern arguments start with #\: (unquoted) as keywords to
support trivially passing to a keyword-aware command with CALL-INTERACTIVELY."
  (mapcar 
   (lambda (x) 
     (if (char= #\: (schar0 x)) 
         (let ((*read-eval* nil)) (read-from-string x))
         x))
   (args)))

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
     (if *interactive*
         (sb-ext:enable-debugger)
         (sb-ext:disable-debugger))
     (unwind-protect
          (restart-case 
              (handler-case (progn ,@body)
                (sb-sys:interactive-interrupt (c)
                  (if *interactive*
                      c
                      (sb-ext:exit :code 130))))
            (exit ()
              :report "Exit SBCL (killing the process)."
              ;; :test (lambda (c) (declare (ignore c)) t)
              (log:trace! "falling through to EXIT from pre-REPL RESTART-CASE~&")
              (sb-ext:exit :code 1))))
     (sb-impl::flush-standard-output-streams)
     (unless *interactive* (sb-ext:exit :code 0))
     ;; reset terminal state
     #+nil (.ris)))

;;; Protocol
(defgeneric print-version (self &optional stream)
  (:documentation "Print the version of SELF."))

;;; CLI Commands
(defkernel cli-command (command) ()
  (:documentation "Class of COMMANDs which may be executed directly from the command line."))

(defmethod make-load-form ((self cli-command) &optional env)
  (make-load-form-saving-slots self :environment env))

(defmethod call :before ((self cli-command) args)
  (log:trace! "calling command: ~A~@[ with args ~A~]~%" self args))

;;; CLI Command Types
(define-command-type (:cli string) (&optional (prompt "Input: "))
  (princ prompt *query-io*)
  (force-output *query-io*)
  (string (read-arg *query-io*)))

(define-command-type (:cli string*) (&optional (prompt "Input: "))
  (mapcar 'string (read-args *command-io* prompt)))

(define-command-type (:cli ustring) (&optional (prompt "INPUT: "))
  (string-upcase (read-arg *command-io* prompt)))

(define-command-type (:cli ustring*) (&optional (prompt "Input: "))
  (let ((*query-io* (if (streamp *command-io*) *command-io* *query-io*)))
    (princ prompt *query-io*)
    (force-output *query-io*)
    (mapcar 'string-upcase (read-args *query-io*))))

(define-command-type (:cli dstring) (&optional (prompt "input: "))
  (princ prompt *query-io*)
  (force-output *query-io*)
  (string-downcase (read-arg *query-io*)))

(define-command-type (:cli *) (&optional (prompt "Input: "))
  (princ prompt *query-io*)
  (force-output *query-io*)
  (read-args *query-io*))

(define-command-type (:cli y/n) (&optional prompt)
  (let ((*query-io* (if (streamp *command-io*) *command-io* *query-io*)))
    (y-or-n-p prompt)))

(define-command-type (:cli yes-or-no) (&optional prompt)
  (let ((*query-io* (if (streamp *command-io*) *command-io* *query-io*)))
    (yes-or-no-p prompt)))

(define-command-type (:cli char) (&optional (prompt "Character: "))
  (princ prompt *query-io*)
  (force-output *query-io*)
  (read-char *query-io*))

(define-command-type (:cli num) (&optional (prompt "Number: "))
  (princ prompt *query-io*)
  (force-output *query-io*)
  (parse-number (read-arg *query-io*)))

(define-command-type (:cli password) (&optional (prompt "Password: "))
  (format *query-io* prompt)
  (force-output *query-io*)
  (without-echo
    (string (read-arg *query-io*))))

(define-command-type (:cli command) (&optional (prompt "Command: "))
  (format *command-io* prompt)
  (force-output *command-io*)
  (when-let ((cmd (read-arg *command-io*)))
    (command cmd)))

;; REVIEW 2026-03-03: 
;; (define-command-type (:cli key)) ;; typed keyword?

;;; CLI
(defstruct cli
  ;; name slot defaults to *package*, must be string
  (name (string-downcase (package-name *package*)) :type string)
  (version "0.1.0")
  description
  (cd *default-pathname-defaults*)
  (hook (make-instance 'key-hook))
  (main (required-argument :main) :type function))

(defmethod version ((self cli)) (cli-version self))
(defmethod name ((self cli)) (cli-name self))
(defaccessor kernel ((self cli)) (cli-main self))

(defcommand (:cli :help) (&optional (arg *cli*))
  "Print help and exit."
  (declare (interactive (ustring "Command: ")))
  (print-help
   (if (cli-p arg) arg
       (ifret (command arg) (undefined-command arg)))))

(defcommand (:cli :version) (&optional (arg *cli*))
  "Print version and exit." 
  (print-version arg *standard-output*))

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
                           :main (if (symbolp ,main) (symbol-function ,main) ,main))))
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

(save :commands :cli)
