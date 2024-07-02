;;; Code:

;;  TODO 2024-05-09: add shell configurables to rules - maybe at sk-command
;;  level. :INPUT :WAIT :OUTPUT
(in-package :std-user)
(defpkg :bin/skel
  (:use :cl :std :cli/clap
   :vc :sb-ext :skel
   :log :dat/sxp)
  (:import-from :cli/shell :*shell-input*)
  (:export :main))

(in-package :bin/skel)
(in-readtable :shell)

(defopt skc-help (print-help $cli) $val)
(defopt skc-version (print-version $cli))
(defopt skc-level *log-level*
  (setq *log-level* (if $val (if (stringp $val)
                                 (sb-int:keywordicate (string-upcase $val))
                                 $val)
                        :info)))

;; TODO 2023-10-13: almost there
;; (defopt skc-config
;;   (init-user-skelrc (when $val (parse-file-opt $val))))

(defcmd skc-edit
  (let ((file (or (when $args (pop $args)) (sk-path *skel-project*))))
    (cli/ed:run-emacsclient (namestring file))))

(defcmd skc-init
  (let ((file (when $args (pop $args)))
	(name (when (> $argc 1) (pop $args)))) ;; TODO: test, may need to be
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
   (if (> $argc 0)
       (find-skelfile (pathname (car $args)) :load t)
       (or *skel-project* *skel-user-config* *skel-system-config*))))


(defcmd skc-inspect
  (sb-ext:enable-debugger)
  (setq *no-exit* t)
  (inspect
   (find-skelfile
    (if $args (pathname (car $args))
	#P".")
    :load t)))

(defcmd skc-id
  (println (std:format-sxhash (obj/id:id (find-skelfile #P"." :load t)))))

(defcmd skc-compile ()
  (sk-call *skel-project* :compile))
(defcmd skc-build ()
  (sk-call *skel-project* :build))
(defcmd skc-dist ()
  (sk-call *skel-project* :dist))
(defcmd skc-install ()
  (sk-call *skel-project* :install))
(defcmd skc-pack ()
  (sk-call *skel-project* :pack))
(defcmd skc-unpack ()
  (sk-call *skel-project* :unpack))
(defcmd skc-bundle ()
  (sk-call *skel-project* :bundle))
(defcmd skc-unbundle ()
  (sk-call *skel-project* :unbundle))
(defcmd skc-clean ()
  (sk-call *skel-project* :clean))
(defcmd skc-test ()
  (sk-call *skel-project* :test))
(defcmd skc-bench ()
  (sk-call *skel-project* :bench))

(defcmd skc-rev
  (case (sk-vc-meta-kind (sk-vc (find-skelfile #P"." :load t)))
    (:hg (progn
           (let ((proc (run-hg-command "id" (list "-i") :stream)))
             (println (read-line (process-output proc))))))
    (:git (progn
            (let ((proc (run-git-command "rev-parse" (list "HEAD") :stream)))
              (println (read-line (process-output proc))))))
    (t (skel-error "unknown VC type"))))

(defun sk-slot-case (sel)
  (std/string:string-case (sel :default (skel-error "invalid slot"))
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
    (":env" (sk-env *skel-project*))
    (":vars" (sk-vars *skel-project*))
    (":include" (sk-include *skel-project*))
    (":stash" (sk-stash *skel-project*))
    (":store" (sk-store *skel-project*))
    (":config" (describe *skel-user-config*))
    (":sys" (describe *skel-system-config*))
    (":cache" (sk-cache *skel-user-config*))))

(defcmd skc-show
  (if $args 
      (mapc (lambda (x) (when-let ((ret (sk-slot-case x))) (println ret))) $args)
      (describe (if (boundp '*skel-project*) *skel-project*
                    (if (boundp '*skel-user-config*) *skel-user-config*
                        (if (boundp '*skel-system-config*) *skel-system-config*
                            (skel-error "skel config files not installed")))))))

(defcmd skc-push
  (case (sk-vc-meta-kind (sk-vc (find-skelfile #P"." :load t)))
    (:git (run-git-command "push" $args t))
    (:hg (run-hg-command "push" $args t))
    (t (skel-error "unknown VC type"))))

(defcmd skc-pull
  (case (sk-vc-meta-kind (sk-vc (find-skelfile #P"." :load t)))
    (:git (run-git-command "pull" $args t))
    (:hg (run-hg-command "pull" (append '("-u") $args) t))
    (t (skel-error "unknown VC type"))))

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
    (:git (run-git-command "clone" $args t))
    (:hg (run-hg-command "clone" $args t))
    (t (skel-error "unknown VC type"))))

(defcmd skc-commit
  ;; (debug! $optc $argc)
  (case (sk-vc-meta-kind (sk-vc (find-skelfile #P"." :load t)))
    (:git (run-git-command "commit" $args t))
    (:hg (run-hg-command "commit" $args t))
    (t (skel-error "unknown VC type"))))

(defcmd skc-make
  (let ((sk (find-skelfile #P"." :load t)))
    (sb-ext:enable-debugger)
    (print $args)
    ;; (setq *no-exit* t)
    (if $args
        (loop for a in $args
              do (debug!
                  (when-let ((rule (sk-find-rule a sk)))
                    (sk-make sk rule))))
        (debug! (sk-make sk (aref (sk-rules sk) 0))))))

(defcmd skc-run
  (if $args
      (mapc (lambda (script)
              (debug!
               (sk-run
                (sk-find-script
                 (pathname-name script)
                 (find-skelfile #P"." :load t))))) $args)
      (required-argument 'name)))

(defcmd skc-vc
  (if $args
      (std/string:string-case ((car $args) :default (skel-error "invalid command"))
        ("status" (skc-status nil nil)))
      (skc-status nil $opts)))

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
  (trace! $args $opts))
  
(define-cli $cli
  :name "skel"
  :version #.(format nil "0.1.1:~A" (read-line (sb-ext:process-output (vc:run-hg-command "id" '("-i") :stream))))
  :description "A hacker's project compiler."
  :thunk skc-show
  :opts (make-opts 
	  (:name "help" :global t :description "print this message" 
	   :thunk skc-help)
	  (:name "version" :global t :description "print version" 
	   :thunk skc-version)
	  (:name "level" :global t :description "set log level (warn,info,debug,trace)"
	   :thunk skc-level)
	  (:name "config" :global t :description "set a custom skel user config" :kind file)
	  (:name "input" :global t :description "input source" :kind string)
	  (:name "output" :global t :description "output target" :kind string))
  :cmds (make-cmds
	  (:name init
	   :description "initialize a skelfile in the current directory"
           :opts (make-opts (:name "name" :description "project name" :kind string))
           :thunk skc-init)
          (:name new
           :description "make a new skel project"
           :opts (make-opts (:name "name" :description "project name" :kind string))
           :thunk skc-new)
          (:name describe
           :description "describe a skelfile"
           :thunk skc-describe)
	  (:name show
	   :description "show project slots"
	   :opts (make-opts 
                   (:name "file" :description "path to skelfile" :kind file)
                   (:name "user" :description "print user configuration")
                   (:name "system" :description "print system configuration"))
	   :thunk skc-show)
          (:name vc
           :description "version control"
           :thunk skc-vc
           :opts (make-opts
                   (:name "root" :description "repository path" :kind directory)))
          (:name id
           :description "print the project id"
           :thunk skc-id)
          (:name rev
           :description "print the current vc revision id"
           :thunk skc-rev)
	  (:name inspect
	   :description "inspect the project skelfile"
	   :opts (make-opts (:name "file" :description "path to skelfile" :kind file))
	   :thunk skc-inspect)
	  (:name make
	   :description "build project targets"
	   :opts (make-opts (:name "target" :description "target to build" :kind string))
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

(defmain ()
  (in-package :sk-user)
  (let ((*log-level* :info))
    (in-readtable :shell)
    (with-cli (opts cmds) $cli
      (init-skel-vars)
      (when-let ((project (find-skelfile #P".")))
        (setq *skel-project* (load-skelfile project)))
      (do-cmd $cli)
      (debug-opts $cli))))
