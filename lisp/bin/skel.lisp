;;; Code:

;;  TODO 2024-05-09: add shell configurables to rules - maybe at sk-command
;;  level. :INPUT :WAIT :OUTPUT
(in-package :std-user)
(defpkg :bin/skel
  (:use :cl :std :cli
   :vc :sb-ext :skel :log :cli/clap/util
   :dat/sxp #+tools :skel/tools/viz)
  (:import-from :cli/shell :*shell-input* :*shell-directory*)
  (:use :cli/tools/sbcl))

(in-package :bin/skel)
(in-readtable :shell)

(defopt skc-help (print-help *cli*))
(defopt skc-version (print-version *cli*))
(defopt skc-level *log-level*
        (setq *log-level* (if *arg* (if (stringp *arg*)
                                        (sb-int:keywordicate (string-upcase *arg*))
                                        *arg*)
                              :info)))

(defopt skc-config (load-user-skelrc (or *arg* *user-skelrc*)))

(defcmd skc-edit
  (let ((file (or (when *args* (pop *args*)) (sk-path *skel-project*))))
    (cli/ed:run-emacsclient (namestring file))))

(defcmd skc-init
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

(defcmd skc-describe
  (describe
   (if (> *argc* 0)
       (find-skelfile (pathname (car *args*)) :load t)
       (or *skel-project* *skel-user-config* *skel-system-config*))))


(defcmd skc-inspect
  (sb-ext:enable-debugger)
  (setq *no-exit* t)
  (inspect
   (find-skelfile
    (if *args* (pathname (car *args*))
	#P".")
    :load t)))

#+tools
(defcmd skc-view
  (if *args* 
      (let ((stuff (loop for a in *args*
                         collect (sk-slot-case a))))
        (sk-view (if (= 1 (length stuff)) (car stuff) stuff)))
      (sk-view (if (boundp '*skel-project*) *skel-project*
                   (if (boundp '*skel-user-config*) *skel-user-config*
                       (if (boundp '*skel-system-config*) *skel-system-config*
                           (skel-simple-error "skel config files not installed")))))))

(defcmd skc-id
  (println (std:format-sxhash (obj/id:id (find-skelfile #P"." :load t)))))

(defun call-with-args (action args)
  (let* ((*default-pathname-defaults* *skel-path*))
    (if (null args)
        (sk-call *skel-project* action)
        (mapc (lambda (x)
                (sk-call *skel-project* (keywordicate (symbol-name action) '- (string-upcase x))))
              args))))

(defcmd skc-compile
  (call-with-args :compile *args*))
(defcmd skc-build
  (call-with-args :build *args*))
(defcmd skc-dist
  (call-with-args :dist *args*))
(defcmd skc-install
  (call-with-args :install *args*))
(defcmd skc-pack
  (call-with-args :pack *args*))
(defcmd skc-unpack
  (call-with-args :unpack *args*))
(defcmd skc-bundle
  (call-with-args :bundle *args*))
(defcmd skc-unbundle
  (call-with-args :unbundle *args*))
(defcmd skc-clean
  (call-with-args :clean *args*))
(defcmd skc-test
  (call-with-args :test *args*))
(defcmd skc-bench
  (call-with-args :bench *args*))
(defcmd skc-save
  (call-with-args :save *args*))

(defun sk-slot-case (sel)
  (std/string:string-case (sel :default (skel-simple-error "invalid slot"))
    (":id" (std:format-sxhash (obj/id:id *skel-project*)))
    (":name" (sk-name *skel-project*))
    (":author" (sk-author *skel-project*))
    (":version" (sk-version *skel-project*))
    (":description" (sk-description *skel-project*))
    (":tags" (sk-tags *skel-project*))
    (":license" (sk-license *skel-project*))
    (":vc" (sk-vc *skel-project*))
    (":components" (sk-components *skel-project*))
    (":scripts" (sk-scripts *skel-project*))
    (":rules" (sk-rules *skel-project*))
    (":phases" (hash-table-alist (sk-phases *skel-project*)))
    (":env" (sk-env *skel-project*))
    (":bind" (sk-bind *skel-project*))
    (":include" (sk-include *skel-project*))
    (":stash" (sk-stash *skel-project*))
    (":store" (sk-store *skel-project*))
    (":config" *skel-user-config*)
    (":sys" *skel-system-config*)
    (":cache" (sk-cache *skel-user-config*))))

(defcmd skc-show
  (if *args*
      (mapc (lambda (x) (when-let ((ret (sk-slot-case x))) (println ret))) *args*)
      (describe (if (boundp '*skel-project*) *skel-project*
                    (if (boundp '*skel-user-config*) *skel-user-config*
                        (if (boundp '*skel-system-config*) *skel-system-config*
                            (skel-simple-error "skel config files not installed")))))))

(defcmd skc-push
  (case (sk-vc-meta-kind (sk-vc (find-skelfile #P"." :load t)))
    (:git (run-git-command "push" *args* t))
    (:hg (run-hg-command "push" *args* t))
    (t (skel-simple-error "unknown VC type"))))

(defcmd skc-pull
  (case (sk-vc-meta-kind (sk-vc (find-skelfile #P"." :load t)))
    (:git (run-git-command "pull" *args* t))
    (:hg (run-hg-command "pull" (append '("-u") *args*) t))
    (t (skel-simple-error "unknown VC type"))))

(defun hg-status ()
  (with-open-stream (proc (process-output (run-hg-command "status" nil :stream)))
    (loop for x = (read-line proc nil)
          while x
          do (println x))))

(defun git-status ()
  (with-open-stream (proc (run-git-command "status" nil :stream))
    (loop for x = (read-line proc nil)
          while x
          do (println x))))

(defcmd skc-status
  (case (sk-vc-meta-kind (sk-vc (find-skelfile #P"." :load t)))
    (:git (git-status))
    (:hg (hg-status))
    (t (hg-status))))

(defcmd skc-clone
  (case (sk-vc-meta-kind (sk-vc (find-skelfile #P"." :load t)))
    (:git (run-git-command "clone" *args* t))
    (:hg (run-hg-command "clone" *args* t))
    (t (skel-simple-error "unknown VC type"))))

(defcmd skc-commit
  ;; (debug! *optc* *argc*)
  (case (sk-vc-meta-kind (sk-vc (find-skelfile #P"." :load t)))
    (:git (run-git-command "commit" *args* t))
    (:hg (run-hg-command "commit" *args* t))
    (t (skel-simple-error "unknown VC type"))))

(defcmd skc-make
  (let ((sk (find-skelfile #P"." :load t)))
    (sb-ext:enable-debugger)
    (log:debug! "cli args" *args*)
    ;; (setq *no-exit* t)
    (if *args*
        (loop for a in *args*
              do (debug!
                  (if-let ((rule (sk-find-rule a sk)))
                    (sk-make sk rule)
                    ;;  TODO 2024-08-23: restart condition here
                    (skel-simple-error "rule not found: ~A" a))))
        (debug! (sk-make sk (aref (sk-rules sk) 0))))))

(defcmd skc-run
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

(defcmd skc-vc
  (if *args*
      (std/string:string-case ((car *args*) :default (skel-simple-error "invalid command"))
        ("status" (skc-status nil nil)))
      (skc-status nil *opts*)))

(defcmd skc-shell
  (sb-ext:enable-debugger)
  (trace! "starting skel shell")
  (setq *no-exit* t)
  (cli/clap::with-cli-handlers
    (progn
      (in-package :sk-user)
      (use-package :cl-user)
      (use-package :sb-ext)
      (use-package :std-user)
      (init-skel-vars)
      (println "Welcome to SKEL")
      (sb-impl::toplevel-repl nil))))

(defcmd skc-new
  (trace! *args* *opts*))

(define-cli *skel-cli*
  :name "skel"
  :version #.(format nil "0.1.1:~A" (read-line (sb-ext:process-output (vc:run-hg-command "id" '("-i") :stream))))
  :description "A hacker's project compiler."
  :thunk 'skc-show
  :opts ((:name "help" :global t :description "print this message" 
	   :thunk skc-help)
	  (:name "version" :global t :description "print version" 
	   :thunk skc-version)
	 (:name "level" :global t :description "set log level (warn,info,debug,trace)"
	  :thunk skc-level)
	 (:name "config" :global t :description "set a custom skel user config" :kind file)
	 (:name "input" :global t :description "input source" :kind string)
	 (:name "output" :global t :description "output target" :kind string))
  :cmds ((:name init
	  :description "initialize a skelfile in the current directory"
          :opts (:name "name" :description "project name" :kind string)
          :thunk skc-init)
         (:name new
          :description "make a new skel project"
          :opts ((:name "name" :description "project name" :kind string))
          :thunk skc-new)
         (:name describe
          :description "describe a skelfile"
          :thunk skc-describe)
	 (:name show
	  :description "show project slots"
	  :opts ((:name "file" :description "path to skelfile" :kind file))
	  :thunk skc-show)
         (:name vc
          :description "version control"
          :thunk skc-vc
          :opts ((:name "root" :description "repository path" :kind directory)))
         (:name id
          :description "print the project id"
          :thunk skc-id)
	 (:name inspect
	  :description "inspect the project skelfile"
	  :opts ((:name "file" :description "path to skelfile" :kind file))
	  :thunk skc-inspect)
         #+tools
         (:name view
          :description "view an object in a new GUI window"
          :thunk skc-view)
	 (:name make
	  :description "build project targets"
	  :opts ((:name "target" :description "target to build" :kind string))
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
          :description "print the vc status"
          :thunk skc-status)
	 (:name push
	  :description "push the current project upstream"
	  :thunk skc-push)
	 (:name pull
	  :description "pull the current project from remote"
          :thunk skc-pull)
	 (:name clone
	  :description "clone a remote project"
          :thunk skc-clone)
	 (:name commit
	  :description "commit changes to the project vc"
          :thunk skc-commit)
	 (:name edit
	  :description "edit a project file in emacs."
          :thunk skc-edit)
	 (:name shell
	  :description "open the sk-shell interpreter"
          :thunk skc-shell)))

(defmain start-skel ()
  (in-package :sk-user)
  (let ((*log-level* :info))
    (in-readtable :shell)
    (with-cli (*skel-cli* opts cmds) (cli:args)
      (debug-opts *cli*)
      (init-skel-vars)
      (when-let ((project (find-skelfile #P".")))
        (let ((*default-pathname-defaults* (pathname (directory-namestring project))))
          (setq *skel-project* (load-skelfile project))
          (setq *skel-path* (sk-src *skel-project*))
          (setq cli/shell:*shell-directory* (sk-src *skel-project*))))
      (do-cmd *cli*))))
