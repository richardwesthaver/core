;;; cli/clap/cli.lisp --- Clap CLI Class

;; Top-level command object of a CLI App

;;; Code:
(in-package :cli/clap/obj)

(defun make-cli (kind &rest slots)
  "Creates a new CLI object of the given kind."
  (declare (type (member :opt :cmd :cli t) kind))
  (cond
    ((eql kind :cli) (apply #'make-instance 'cli slots))
    ((eql kind :opt) (apply #'make-cli-opt slots))
    ((eql kind :cmd) (apply #'make-instance 'cli-cmd slots))
    (t (apply #'make-instance kind slots))))

(defopt help-opt (print-help *cli*))
(defopt version-opt (print-version *cli*))
(defopt level-opt
  (setq *log-level* (if *arg* 
                        (if (stringp *arg*)
                            (sb-int:keywordicate (string-upcase *arg*))
                            *arg*)
                        *log-level*)))

(defmacro define-cli (sym &key name version help description thunk opts cmds include)
  "Define a symbol SYM bound to a top-level CLI object.

NAME is assigned to the CLI and assumed to be the default binary name which
uses this object.

VERSION, DESCRIPTION, and THUNK are assigned to the associated slot value of
the CLI as is.

When HELP is non-nil, auto-generate a '--help' CLI-OPT and assign it to this
object.

OPTS and CMDS are lists of forms which are passed directly to MAKE-CLI :OPT
and MAKE-CLI :CMD respectively.

INCLUDE is similar to the DEFSTRUCT keyword of the same name and specifies
that some or all of the slots of a CLI object should be inherited by this one."
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
                `((:name "help" :description "print help"
                   :thunk cli/clap/obj::help-opt))
                opts))
              (make-opts opts)))
    ;; TODO (when include 
    `(,*default-cli-def* ,%name (make-cli ,%class :name ,name
                                                  :version ,version
                                                  :description ,description
                                                  :thunk ',thunk
                                                  :opts ,%opts
                                                  :cmds ,(make-cmds cmds)))))

(defmacro defmain (name (&key (exit t)) &body body)
  "Define a CLI main function in the current package."
  (multiple-value-bind (body decls docs) (parse-body body :documentation t)
    `(let ((*no-exit* ,(not exit)))
       (defun ,name ()
         ,(or docs "Run the top-level function and print to *STDOUT*.")
         ,@decls
         (with-cli-handlers
           (progn
             ,@body))))))

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

(defclass cli (cli-cmd)
  ;; name slot defaults to *package*, must be string
  ((name :initarg :name :initform (string-downcase (package-name *package*)) :accessor cli-name :type string)
   (version :initarg :version :initform "0.1.0" :accessor cli-version :type string)
   ;; TODO 2023-10-11: look into pushd popd - cd-stack?
   (cd :initarg :cd :initform (sb-posix:getcwd) :type string :accessor cli-cd
       :documentation "working directory of the top-level CLI."))
  (:documentation "CLI"))

(defmethod print-usage ((self cli) &optional stream)
  (iprintln (format nil "usage: ~A [opts] <command> [<arg>]~%" (cli-name self)) 2 stream))

(defmethod print-version ((self cli) &optional stream)
  (println (cli-version self) stream))

(defmethod print-help ((self cli) &optional (stream t)) 
  (println (format nil "~A v~A --- ~A~%" (cli-name self) (cli-version self) (cli-description self)) stream)
  (print-usage self stream)
  ;; (terpri stream)
  (println "options:" stream)
  (with-slots (opts cmds) self
    (unless (null opts)
      (loop for o across opts
            do (iprintln (print-usage o nil) 2 stream)))
    (terpri stream)
    (println "commands:" stream)
    (unless (null cmds)
      (loop for c across cmds
            do (iprintln (print-usage c nil) 2 stream)))))

(defmethod cli-equal :before ((a cli) (b cli))
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

(defmacro with-cli ((cli &key slots (args *args*) (install t) run exit) &body body)
  "Like with-slots with some extra bindings.

- CLI is an instance of a CLI class.

- SLOTS is a list passed directly to WITH-SLOTS with CLI.

- ARGS is a list of arguments to parse with this cli object.

- INSTALL defaults to T which implies that the AST will be consumed before
  BODY. A nil value indicates that the AST will not be consumed and it is up
  to the user to provide a binding for the AST slot so that they may call
  INSTALL-AST manually. Alternatively a special value :AFTER may be supplied
  which will delay installation until after BODY is evaluated.

 - RUN with a non-nil value will call DO-CMD on the CLI after evaluating BODY.

- EXIT with a non-nil value will exit the current process as the last hook
  after evaluating BODY.

CLI is updated based on the current environment and dynamically bound to
*CLI*. ARGS is a list of CLI args, defaults to *ARGS* at runtime if nil. *AST* is bound to the parsed result of"
  `(progn
     (let ((*cli* ,cli))
       (setf (cli-cd *cli*) *default-pathname-defaults*)
       (let ((*args* ,args)
             (*ast* (proc-args ,cli ,args)))
         ,@(when (eql install t)
             `((install-ast *cli* *ast*)))
         (with-slots ,slots *cli*
           ,@body
           ,@(when (eql install :after) '((install-ast *cli*)))
           ,@(when run '((do-cmd *cli*)))
           ,@(when exit '((sb-ext:exit))))))))

(defmacro with-cli-args (args &body body)
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
