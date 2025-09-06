;;; cli.lisp --- Skel CLI Definitions

;; CLI implementation of Skel

;;; Code:
(in-package :skel/cli)

;; *SKEL-CLI* exposes access to the SKEL system from the CLI.
(defcmd skc-init ()
  (let ((file (when *args* (pop *args*)))
        (name (when (> *argc* 1) (pop *args*))))
    ;; TODO: test, may need to be sequential for side-effect of pop
    (handler-bind
        ((sb-ext:file-exists
           #'(lambda (s)
               (std:println (format nil "file already exists: ~A" 
                                    (or file *default-skelfile*)))
               (let ((f2 (read-line)))
                 (if (string= f2 "") 
                     (error s)
                     (use-value f2 s))))))
      (init-skelfile file name))))

(defcmd skc-describe ()
  (describe
   (if (> *argc* 0)
       (find-skelfile (pathname (car *args*)) :load t)
       (or *skel-project* *skel-user-config* *skel-system-config*))))

(defcmd skc-inspect ()
  (sb-ext:enable-debugger)
  (setq *no-exit* t)
  (inspect (or *skel-project* *skel-user-config*)))

(defun call-with-args (action args)
  (let* ((*default-pathname-defaults* *skel-path*))
    (if (null args)
        (sk-call *skel-project* action)
        (mapc (lambda (x)
                (sk-call *skel-project* (keywordicate (symbol-name action) '- (string-upcase x))))
              args))))

(defcmd skc-compile ()
  (call-with-args :compile *args*))
(defcmd skc-build ()
  (call-with-args :build *args*))
(defcmd skc-update ()
  (call-with-args :update *args*))
(defcmd skc-dist ()
  (call-with-args :dist *args*))
(defcmd skc-install ()
  (call-with-args :install *args*))
(defcmd skc-pack ()
  (call-with-args :pack *args*))
(defcmd skc-unpack ()
  (call-with-args :unpack *args*))
(defcmd skc-bundle ()
  (call-with-args :bundle *args*))
(defcmd skc-unbundle ()
  (call-with-args :unbundle *args*))
(defcmd skc-clean ()
  (call-with-args :clean *args*))
(defcmd skc-test ()
  (call-with-args :test *args*))
(defcmd skc-bench ()
  (call-with-args :bench *args*))
(defcmd skc-save ()
  (call-with-args :save *args*))

(defcmd skc-show ()
  (if *args*
      (mapc (lambda (x) 
              (let ((y (string-left-trim ":" x)))
                (if (sk-project-slot y nil)
                    (sk-print
                     (slot-value
                      *skel-project*
                      (sb-mop:slot-definition-name
                       (find y 
                             (sb-mop:class-slots (class-of *skel-project*))
                             :test 'string=
                             :key (lambda (x) (string-downcase (sb-mop:slot-definition-name x)))))))
                    (log:fatal! "unknown argument: ~A~%" x))))
            *args*)
      (cond
        ((boundp '*skel-project*)
         (sk-print *skel-project* :exclude (if ast:*keep-ast* '(:ast :rules) '(:rules))))
        ((boundp '*skel-user-config*) (sk-print *skel-user-config*))
        ((boundp '*skel-system-config*) (sk-print *skel-system-config*))
        (t (skel-simple-error "skel not installed")))))

(defcmd skc-list ()
  (if (zerop *argc*)
      (list-all-projects)
      (string-case ((subseq (pop *args*) 0 3))
        ("pro" (list-all-projects))
        ("log" (apply 'sk-log-list *args*)))))

(defcmd skc-id ()
  (println (octet-vector-to-hex-string (integer-to-octets (id:id *skel-project*)))))

(defopt skc-config (load-user-skelrc (or *arg* *user-skelrc*) nil))

(defcmd skc-edit ()
  (let ((file (or (when *args* (pop *args*)) (path *skel-project*))))
    (cli/ed:run-emacsclient (namestring file))))

(defcmd skc-make ()
  (let ((sk *skel-project*))
    (sb-ext:enable-debugger)
    (if *args*
        (loop for a in *args*
              do (debug!
                  (if-let ((rule (sk-find a sk)))
                    (sk-make sk rule)
                    ;;  TODO 2024-08-23: restart condition here
                    (skel-simple-error "rule not found: ~A" a))))
        (sk-make sk (aref (sk-rules sk) 0)))))

(defcmd skc-status ()
  (vc:vc-status (sk-vc *skel-project*)))

(defcmd skc-run ()
  (sb-ext:enable-debugger)
  (if *args*
      (mapc (lambda (script)
              ;; first check if a script with the same name exists, else check
              ;; for a rule definition
              (if-let ((script (sk-find
                                (pathname-name script)
                                *skel-user-config*)))
                (sk-run script)
                (call-with-args :run (list script))))
            *args*)
      (required-argument 'name)))

(defcmd skc-new ()
  (println *args*)
  (println *opts*))

(defcmd skc-search ()
  "Search in the current project."
  (dolist (a *args*)
    (println (sk-search-project a))))

(defcmd skc-start ()
  "Start a service."
  (dolist (a *args*)
    (println (srv:make-service (keywordicate (string-upcase a))))))

(defun sk-shell ()
  (trace! "starting skel shell")
  (setq *no-exit* t)
  (progn
    (in-package :sk-user)
    (use-package :cl-user)
    (use-package :sb-ext)
    (use-package :std-user)
    (println "Welcome to SKEL")
    (cli/linedit:install-repl :wrap-current t :history "/tmp/skel.history" :killring "/tmp/skel.killring")
    (cli/repl:make-toplevel-init
     :package :sk-user
     :userinit (lambda () (merge-homedir-pathnames ".corerc"))
     :default t)))

(defcmd skc-shell () (sk-shell))

(define-cli *skel-cli*
  :help t
  :version (format nil "0.1.1:~A" (read-line (sb-ext:process-output (vc:run-hg-command "id" '("-i") :stream))))
  :description "The hackable devtool."
  :thunk skc-show
  :name "skel"
  :opts 
  ((:name "version" 
    :description "print version"
    :kind boolean
    :thunk version-opt)
   (:name "ast" :description "save the intermediate skel AST" 
    :thunk keep-ast-opt :kind boolean)
   (:name "level" :description "set log level (warn,info,debug,trace)"
    :thunk level-opt)
   (:name "config" :description "set a custom skel user config" 
    :kind file 
    :thunk skc-config))
  :cmds 
  ((:name init
    :description "initialize a skelfile in the current directory"
    :opts ((:name "name" :description "project name" :kind string))
    :thunk skc-init)
   (:name start
    :description "start a skel service"
    :thunk skc-start)
   (:name id
    :description "print the current project id"
    :thunk skc-id)
   (:name inspect
    :description "inspect the project skelfile"
    :opts ((:name "file" :description "path to skelfile" :kind file))
    :thunk skc-inspect)
   (:name new
    :description "make a new skel project"
    :opts ((:name "name" :description "project name" :kind string))
    :thunk skc-new)
   (:name describe
    :description "describe a skelfile"
    :thunk skc-describe)
   (:name edit
    :description "edit a project file in emacs."
    :thunk skc-edit)
   (:name show
    :description "show skel objects slots"
    :opts ((:name "file" :description "path to skelfile" :kind file))
    :thunk skc-show)
   (:name status
    :description "show the current project status"
    :thunk skc-status)
   (:name list
    :description "list skel objects"
    :thunk skc-list)
   (:name make
    :description "build project targets"
    :thunk skc-make)
   (:name search
    :description "search the current project"
    :thunk skc-search)
   (:name run
    :description "run a script or command"
    :thunk skc-run)
   (:name compile
    :description "compile source code"
    :thunk skc-compile)
   (:name build
    :description "build programs and libraries"
    :thunk skc-build)
   (:name update
    :description "update components"
    :thunk skc-update)
   (:name save
    :description "save a file"
    :thunk skc-save)
   (:name dist
    :description "distribute build artifacts"
    :thunk skc-dist)
   (:name install
    :description "install stuff"
    :thunk skc-install)
   (:name pack
    :description "pack stuff"
    :thunk skc-pack)
   (:name unpack
    :description "unpack stuff"
    :thunk skc-unpack)
   (:name bundle
    :description "bundle source code"
    :thunk skc-bundle)
   (:name unbundle
    :description "unbundle source code"
    :thunk skc-unbundle)
   (:name clean
    :description "clean up the project"
    :thunk skc-clean)
   (:name test
    :description "run tests"
    :thunk skc-test)
   (:name bench
    :description "run benchmark"
    :thunk skc-bench)
   (:name shell
    :description "open the sk-shell interpreter"
    :thunk skc-shell)))
