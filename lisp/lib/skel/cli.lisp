;;; cli.lisp --- Skel CLI Definitions

;; CLI implementation of Skel

;;; Code:
(in-package :skel/cli)

;; *SKEL-CLI* exposes access to the SKEL system from the CLI.
(defcmd skc-init ()
  (let ((file (when *args* (pop *args*)))
        (name (when (> *argc* 1) (pop *args*))))
    ;; TODO: test, may need to be
    ;; sequential for side-effect
    ;; of pop
    (handler-bind
        ((sb-ext:file-exists
           #'(lambda (s)
               (std:println (format nil "file already exists: ~A" (or file *default-skelfile*)))
               (let ((f2 (read-line)))
                 (if (string= f2 "") 
                     (error s)
                     (use-value f2 s))))))
      (init-skelfile file name))))

(defcmd skc-status ()
  (vc:vc-status (sk-vc (find-skelfile #P"." :load t))))

(defcmd skc-describe ()
  (describe
   (if (> *argc* 0)
       (find-skelfile (pathname (car *args*)) :load t)
       (or *skel-project* *skel-user-config* *skel-system-config*))))

(defcmd skc-inspect ()
  (sb-ext:enable-debugger)
  (setq *no-exit* t)
  (inspect
   (find-skelfile
    (if *opts* (cli-opt-val (aref *opts* 0))
        #P".")
    :load t)))

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

(defun sk-slot-case (sel)
  (std/string:string-case ((string-left-trim ":" sel) :default (skel-simple-error "invalid slot"))
    ("id" (std:format-sxhash (obj/id:id *skel-project*)))
    ("name" (name *skel-project*))
    ("author" (sk-author *skel-project*))
    ("version" (sk-version *skel-project*))
    ("description" (sk-description *skel-project*))
    ("tags" (format nil "~{~A~^ ~}" (sk-tags *skel-project*)))
    ("license" (sk-license *skel-project*))
    ("vc" (sk-vc *skel-project*))
    ("components" (sk-components *skel-project*))
    ("scripts" (sk-scripts *skel-project*))
    ("rules" (sk-rules *skel-project*))
    ("phases" (hash-table-alist (sk-phases *skel-project*)))
    ;; ("env" (sk-env *skel-project*))
    ("bind" (sk-bind *skel-project*))
    ("include" (sk-include *skel-project*))
    ("stash" (sk-stash *skel-project*))
    ("store" (sk-store *skel-project*))
    ("config" *skel-user-config*)
    ("sys" *skel-system-config*)
    ("cache" (sk-cache *skel-user-config*))))

(defcmd skc-show ()
  (if *args*
      (mapc (lambda (x) (when-let ((ret (sk-slot-case x))) (println ret))) *args*)
      (sk-print (if (boundp '*skel-project*) *skel-project*
                    (if (boundp '*skel-user-config*) *skel-user-config*
                        (if (boundp '*skel-system-config*) *skel-system-config*
                            (skel-simple-error "skel config files not installed")))))))

(defopt skc-version (print-version *cli* t))
(defopt skc-ast (setq *keep-ast* (or *arg*)))
(defopt skc-level *log-level*
        (setq *log-level* (if *arg* (if (stringp *arg*)
                                        (sb-int:keywordicate (string-upcase *arg*))
                                        *arg*)
                              :info)))

(defopt skc-config (load-user-skelrc (or *arg* *user-skelrc*)))

(defcmd skc-edit ()
  (let ((file (or (when *args* (pop *args*)) (path *skel-project*))))
    (cli/ed:run-emacsclient (namestring file))))

#+gui
(defcmd skc-view ()
  (if *args* 
      (let ((stuff (loop for a in *args*
                         collect (sk-slot-case a))))
        (sk-view (if (= 1 (length stuff)) (car stuff) stuff)))
      (sk-view (if (boundp '*skel-project*) *skel-project*
                   (if (boundp '*skel-user-config*) *skel-user-config*
                       (if (boundp '*skel-system-config*) *skel-system-config*
                           (skel-simple-error "skel config files not installed")))))))

(defcmd skc-id ()
  (println (std:format-sxhash (obj/id:id (find-skelfile #P"." :load t)))))

(defcmd skc-make ()
  (let ((sk (find-skelfile #P"." :load t)))
    (sb-ext:enable-debugger)
    (if *args*
        (loop for a in *args*
              do (debug!
                  (if-let ((rule (sk-find-rule a sk)))
                    (sk-make sk rule)
                    ;;  TODO 2024-08-23: restart condition here
                    (skel-simple-error "rule not found: ~A" a))))
        (debug! (sk-make sk (aref (sk-rules sk) 0))))))

(defcmd skc-run ()
  (if *args*
      (mapc (lambda (script)
              ;; first check if a script with the same name exists, else check for a rule definition
              (if-let ((script (sk-find-script 
                                (pathname-name script)
                                (find-skelfile #P"." :load t))))
                (sk-run script)
                (call-with-args :run (list script))))
            *args*)
      (required-argument 'name)))

(defcmd skc-new ()
  (println *args*)
  (println *opts*))

(define-cli *skel-cli*
  :help t
  :version (format nil "0.1.1:~A" 
                   (read-line (sb-ext:process-output (vc:run-hg-command "id" '("-i") :stream))))
  :description "The hackable devtool."
  :thunk skc-show
  :name "skel"
  :opts ((:name "version" :description "print version" 
          :thunk skc-version)
         (:name "ast" :description "save the intermediate skel AST" :thunk skc-ast)
         (:name "level" :description "set log level (warn,info,debug,trace)"
          :thunk skc-level)
         (:name "config" :description "set a custom skel user config" :kind file))
  :cmds ((:name init
          :description "initialize a skelfile in the current directory"
          :opts ((:name "name" :description "project name" :kind string))
          :thunk skc-init)
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
          :description "show project slots"
          :opts ((:name "file" :description "path to skelfile" :kind file))
          :thunk skc-show)
         (:name id
          :description "print the project id"
          :thunk skc-id)
         #+gui
         (:name view
          :description "view an object in a new window"
          :thunk skc-view)
         (:name make
          :description "build project targets"
          :thunk skc-make)
         (:name run
          :description "run a script or command"
          :thunk skc-run)
         (:name compile
          :description "compile source code"
          :thunk skc-compile)
         (:name build
          :description "build programs and libraries"
          :thunk skc-build)
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
         (:name status
          :description "print the project status"
          :thunk skc-status)))
