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
                         "Skip to toplevel READ/EVAL/PRINT loop."
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
(defgeneric print-help (self &optional stream)
  (:documentation "Format command SELF as a helpful string."))

(defgeneric print-version (self &optional stream)
  (:documentation "Print the version of SELF."))

(defgeneric print-usage (self &optional stream)
  (:documentation "Format command SELF as a useful string."))

;;; CLI Command
(defkernel cli-command (command) ())

(defmethod make-load-form ((self cli-command) &optional env)
  (make-load-form-saving-slots self :environment env))

(init :commands :name :cli :class 'cli-command)

#+todo
(defmethod print-usage ((self cli-command) &optional stream)
  (with-slots (opts cmds) self
    (format stream "~(~A~)~:[~;*~]~24t~@[~A~]~@[~{~%~4t~A~^~}~]~@[~{~A~}~]~&"
            (name self)
            (when *cli*
              (equal (string (kernel *cli*)) (string (kernel self))))
            (kernel-documentation self)
            (unless (sequence:emptyp opts)
              (loop for o across opts collect (with-output-to-string (s) (print-usage o s))))
            (unless (sequence:emptyp cmds)
              (loop for c across cmds collect (with-output-to-string (s) (print-usage c s)))))))

#+todo
(defmethod print-help ((self cli-command) &optional stream)
  (unless (typep self 'cli)
    (print-usage self stream))
  (let ((opts (opts self))
        (cmds (cmds self)))
    (unless (sequence:emptyp opts)
      (println "options:" stream)
      (loop for o across opts
            do (iprintln (with-output-to-string (s) (print-usage o s)) 2 stream)))
    (terpri stream)
    (unless (sequence:emptyp cmds)
      (println "commands:" stream)
      (loop for c across cmds
            do (iprintln (with-output-to-string (s) (print-usage c s)) 2 stream)))))

(defmethod call :before ((self cli-command) args)
  (log:trace! "calling command: ~A~@[ with args ~A~]~%" self args))

;;; CLI
(defcommand (:cli :help) (&optional arg)
  "Print help and exit."
  (print-help (if arg (command arg) *cli*) t)
  (sb-ext:exit :code 0))

(defcommand (:cli :version) (&optional arg)
  "Print version and exit." 
  (print-version (if arg (command arg) *cli*) t)
  (sb-ext:exit :code 0))

(define-command-type (:cli :keep-ast) ()
  "Set the *KEEP-AST* variable."
  (setq ast:*keep-ast* t))

(define-command-type (:cli :level) (&optional arg)
  "Set the *LOG-LEVEL* for this CLI session."
  (if arg
      (setq *log-level* (if (stringp arg)
                            (sb-int:keywordicate (string-upcase arg))
                            arg))
      *log-level*))

;; REVIEW 2026-01-16: should this be a struct containing a CLI-COMMAND? hmm..
(defstruct cli
  ;; name slot defaults to *package*, must be string
  (name (string-downcase (package-name *package*)) :type string :read-only t)
  (version "0.1.0" :read-only t)
  description
  (cd *default-pathname-defaults*)
  (hook (make-instance 'key-hook))
  (kernel (with-commands :cli (command 'help))))

(defmethod version ((self cli)) (cli-version self))
(defmethod name ((self cli)) (cli-name self))
(defaccessor kernel ((self cli)) (cli-kernel self))

(defmacro define-cli (name 
                      &key (version "0.1.0") 
                           (kernel (with-commands :cli (command :help)))
                           hook
                           (cd *default-pathname-defaults*)
                           description)
  "Define a new CLI object.

NAME is assigned to the CLI and assumed to be the default binary name which
uses this object.

VERSION, DESCRIPTION, and KERNEL are assigned to the associated slot
value of the CLI."
  `(let* ((*cli* (make-cli :name ,name :version ,version :hook ,hook :cd ,cd :description ,description)))
     (setf (cli-kernel *cli*) ,kernel)
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
