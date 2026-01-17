;;; cli/clap/cli.lisp --- Clap CLI Class

;; Top-level command object of a CLI App

;;; Code:
(in-package :cli/clap)

(defun make-cli (type &rest slots)
  "Creates a new CLI object of the given cli type."
  (declare ((member :opt :cmd :cli t) type))
  ;; (print (getf slots :thunk))
  (cond
    ((eql type :cli) (apply #'make-instance 'cli slots))
    ;; replace :DEFAULT with :VAL
    ((eql type :opt) (apply #'make-cli-opt (substitute :val :default slots)))
    ((eql type :cmd) (apply #'make-instance 'cli-cmd slots))
    (t (apply #'make-instance type slots))))

(defopt help-opt 
  "Print help and exit."
  (if *arg*
      (progn
        (print-help (find-cmd *arg* *cli* :default :error) t)
        (terpri t))
      (print-help *cli*))
  (exit :code 0))

(defopt version-opt 
  "Print version and exit." 
  (print-version *cli*)
  (exit :code 0))

(defopt keep-ast-opt
  "Set the *KEEP-AST* variable."
  (setq ast:*keep-ast* t))

(defopt level-opt
  "Set the *LOG-LEVEL* for this CLI session."
  (if *arg*
      (setq *log-level* (if (stringp *arg*)
                            (sb-int:keywordicate (string-upcase *arg*))
                            *arg*))
      *log-level*))

(defmacro define-cli (sym &key name version help description thunk opts cmds package)
  "Define a symbol SYM bound to a top-level CLI object.

NAME is assigned to the CLI and assumed to be the default binary name which
uses this object.

VERSION, DESCRIPTION, and THUNK are assigned to the associated slot value of
the CLI as is.

When HELP is non-nil, auto-generate a '--help' CLI-OPT and assign it to this
object.

OPTS and CMDS are lists of forms which are passed directly to MAKE-CLI :OPT
and MAKE-CLI :CMD respectively."
  (with-gensyms (%name %class %opts)
    (if (atom sym)
        (setq %name sym
              %class :cli)
        (setq %name (car sym)
              %class (cdr sym)))
    (setq %opts
          (if help
              (make-opts
               (append
                `((:name "help" :description "print help" :type string
                   :thunk cli/clap/obj::help-opt))
                opts))
              (make-opts opts)))
    `(prog1 (,*default-cli-def* ,%name (make-cli ,%class :name ,name
                                                         :version ,version
                                                         :description ,description
                                                         :thunk ',thunk
                                                         :opts ,%opts
                                                         :cmds ,(make-cmds cmds)))
       ,@(when package `((load-package-cli ,%name :package ,package))))))

;; (defmacro defcli ())?

;; RESEARCH 2023-09-12: closed over hash-table with short/long flags
;; to avoid conflicts. if not, need something like a flag-function
;; slot at class allocation.
(defun make-opts (opts)
  "Make a vector of CLI-OPTs based on OPTS."
  (map 'vector
       (lambda (x)
         (etypecase x
           (string (make-cli-opt :name x))
           (list (apply #'make-cli :opt x))
           (symbol (make-cli-opt :name (string-downcase (symbol-name x ))))))
       opts))

(defun make-cmds (cmds)
  "Make a vector of CLI-CMDs based on CMDS."
  (map 'vector
       (lambda (x)
         (etypecase x
           (cli-cmd x)
           (string (make-cli :cmd :name x))
           (list (apply #'make-cli :cmd x))
           (t (make-cli :cmd :name (format nil "~(~A~)" x)))))
       cmds))

(defkernel cli (cli-command)
  ;; name slot defaults to *package*, must be string
  ((name :initarg :name :initform (string-downcase (package-name *package*)) :accessor name :type string)
   (version :initarg :version :initform "0.1.0" :accessor version :type string)
   ;; TODO 2023-10-11: look into pushd popd - cd-stack?
   (cd :initarg :cd :initform (sb-posix:getcwd) :type string :accessor cli-cd
       :documentation "working directory of the top-level CLI.")
   (hook :initarg :hook :type hook :accessor hook))
  (:documentation "CLI"))

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
  (with-slots (version) a
    (with-slots ((bv version)) b
      (string= version bv))))

(declaim (inline debug-opts))
(defun debug-opts (cli)
  (let ((o (active-opts cli))
        (a (cli-args cli))
        (c (active-cmds cli)))
    (log:debug! :pwd (cli-cd cli) :active-opts o :cmd-args a :active-cmds c)))

(deftype cli-hook-designator () '(or boolean :after))

;; TODO 2025-06-13: call-with-cli
;; (defwith
(defmacro with-cli ((cli &key slots (args *args*) (install t) run exit)  &body body)
  "Like with-slots with some extra bindings.

- CLI is an instance of a CLI class.

- SLOTS is a list passed directly to WITH-SLOTS with CLI.

- ARGS is a list of arguments to parse with this cli object.

- INSTALL defaults to T which implies that the AST will be consumed before
  BODY. A nil value indicates that the AST will not be consumed and it is up
  to the user to provide a binding for the AST slot so that they may call
  WRAP manually. Alternatively a special value :AFTER may be supplied
  which will delay installation until after BODY is evaluated.

 - RUN with a non-nil value will call DO-CMD on the CLI after evaluating BODY.

- EXIT with a non-nil value will exit the current process as the last hook
  after evaluating BODY.

CLI is updated based on the current environment and dynamically bound to
*CLI*. ARGS is a list of CLI args, defaults to *ARGS* at runtime if nil. *AST*
is bound to the parsed result of PROC-ARGS."
  `(progn
     (let ((*cli* ,cli))
       (setf (cli-cd *cli*) *default-pathname-defaults*)
       (let ((*args* ,args)
             (*ast* (proc-args ,cli ,args)))
         ,@(when (eql install t)
             `((wrap *cli* *ast*)))
         (with-slots ,slots *cli*
           ,@body
           ,@(when (eql install :after) '((wrap *cli* *ast*)))
           ,@(when run '((do-cmd *cli*)))
           ,@(when exit '((sb-ext:exit))))))))

(defmacro with-cli-args (args &body body)
  "Bind *ARGS* and *ARGC* from ARGS around BODY."
  `(let ((*args* ,args)
         (*argc* ,(length args)))
     ,@body))

;;; CLI Package Helpers
(defun %package-cli (&optional (package *package*))
  (gethash (package-name package) *cli-package-table*))
(defun (setf %package-cli) (new &optional (package *package*))
  (setf (gethash (package-name package) *cli-package-table*) new))
(defun package-cli (&optional (package *package*))
  (car (%package-cli package)))
(defun (setf package-cli) (new &optional (package *package*))
  (setf (car (%package-cli package)) new))
(defun package-cmds (&optional (package *package*))
  (cadr (%package-cli package)))
(defun (setf package-cmds) (new &optional (package *package*))
  (setf (cadr (%package-cli package)) new))
(defun package-opts (&optional (package *package*))
  (caddr (%package-cli package)))
(defun (setf package-opts) (new &optional (package *package*))
  (setf (caddr (%package-cli package)) new))

(deferror missing-package-cli (simple-error) ()
          (:default-initargs :format-control "Missing PACKAGE-CLI method for ~A"))

(defun missing-package-cli (key)
  (error 'missing-package-cli :format-arguments (list key)))

;; these functions are used to populate a *CLI-PACKAGE-TABLE* record.
(defmacro load-package-cli (cli &key (package *package*) cmds opts)
  (with-gensyms (%cli)
    `(let ((,%cli (if-let ((pkg (and (keywordp ,cli) (package-cli (find-package ,cli)))))
                    (copy-object pkg)
                    (if (typep ,cli 'cli)
                        ,cli
                        (missing-package-cli ,cli)))))
       (setf (cmds ,%cli) (concatenate 'vector (cmds ,%cli) (make-cmds ',cmds))
             (opts ,%cli) (concatenate 'vector (opts ,%cli) (make-opts ',opts)))
       (setf (%package-cli ,package)
             (list ,%cli (cmds ,%cli) (opts ,%cli))))))

(defun add-package-cmd (cmd &optional (package *package*))
  (vector-push-extend cmd (package-cmds package)))

(defun add-package-opt (opt &optional (package *package*))
  (vector-push-extend opt (package-opts package)))

(defmacro add-package-cmds (&rest cmds)
  `(setf (package-cmds *package*) (concatenate 'vector (package-cmds *package*) (make-cmds ',cmds))))

(defmacro add-package-opts (&rest opts)
  `(setf (package-opts *package*) (concatenate 'vector (package-opts *package*) (make-opts ',opts))))
