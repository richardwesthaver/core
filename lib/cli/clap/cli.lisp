;;; cli/clap/cli.lisp --- Clap CLI Class

;; Top-level command object of a CLI App

;;; Code:
(in-package :cli/clap)

(define-command-type (:cli :help) (&optional arg)
  "Print help and exit."
  (print-help (if arg (command arg) *cli*) t)
  (sb-ext:exit :code 0))

(define-command-type (:cli :version) (&optional arg)
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
(defkernel cli (cli-command)
  ;; name slot defaults to *package*, must be string
  ((name :initarg :name :initform (string-downcase (package-name *package*)) :accessor name :type string)
   (version :initarg :version :initform "0.1.0" :accessor version)
   (cd :initarg :cd :initform *default-pathname-defaults* :accessor cli-cd
       :documentation "working directory of the top-level CLI.")
   (hook :initarg :hook :type hook :accessor hook)
   (package :initarg :package :accessor cli-package))
  (:documentation "CLI"))

(defmacro define-cli ((name &key (version "0.1.0") package (help t)) args &body body)
  "Define a new CLI object.

NAME is assigned to the CLI and assumed to be the default binary name which
uses this object.

VERSION, DESCRIPTION, KERNEL, and PACKAGE are assigned to the associated slot
value of the CLI."
  (with-gensyms (%name %class)
    (if (atom name)
        (setq %name name
              %class ''cli)
        (setq %name (car name)
              %class (cdr name)))
    `(let ((cmd::*default-command-class* ,%class))
       (defcommand ,name (,@args &aux (version ,version) ,@(when help '(help)))
         ,@body)
       ,@(when package `((load-package-cli ,%name :package ,package))))))

(defmethod print-usage ((self cli) &optional stream)
  (println (format nil "usage: ~A [opts] <command> [<arg>]~%" (name self)) stream))

(defmethod print-version ((self cli) &optional stream)
  (println (version self) stream))

(deffmt fmt-cli-header "~A v~A --- ~A~%" "Given a NAME VERSION and DESCRIPTION, print a basic cli header.")

(defmethod print-help :before ((self cli) &optional (stream t))
  (fmt-cli-header stream (name self) (version self) (description self)) stream)

(defmethod equiv :before ((a cli) (b cli))
  "Return T if A is the same cli object as B.

Currently this function is intended only for instances of the CLI
class and is used as a specialized EQL for DEFINE-CONSTANT."
  (with-slots (version name ) a
    (with-slots ((bv version) (bn name)) b
      (and (equiv version bv)
           (string= name bn)))))

(definline debug-opts (cli)
  (let ((o (active-opts cli))
        (a (cli-args cli))
        (c (active-cmds cli)))
    (log:debug! :pwd (cli-cd cli) :active-opts o :cmd-args a :active-cmds c)))

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
(defun %package-cli (&optional (package *package*))
  (gethash (package-name package) *cli-package-table*))
(defun (setf %package-cli) (new &optional (package *package*))
  (setf (gethash (package-name package) *cli-package-table*) new))
(defun package-cli (&optional (package *package*))
  (car (%package-cli package)))
(defun (setf package-cli) (new &optional (package *package*))
  (setf (car (%package-cli package)) new))
(defun package-commands (&optional (package *package*))
  (cadr (%package-cli package)))
(defun (setf package-commands) (new &optional (package *package*))
  (setf (cadr (%package-cli package)) new))

(deferror missing-package-cli (simple-error) ()
          (:default-initargs :format-control "Missing PACKAGE-CLI method for ~A"))

(defun missing-package-cli (key)
  (error 'missing-package-cli :format-arguments (list key)))

;; these functions are used to populate a *CLI-PACKAGE-TABLE* record.
(defun load-package-cli (cli &key (package *package*))
  (setf (%package-cli package) 
         (if-let ((pkg (and (keywordp cli) (package-cli (find-package cli)))))
           (copy-object pkg)
           (if (typep cli 'cli)
               cli
               (missing-package-cli cli)))))
             
