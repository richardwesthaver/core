;;; Code:
(uiop:define-package :bin/skel
    (:use :cl :std :cli/clap :vc :sb-ext)
  (:use-reexport :skel :log)
  (:export :main))

(in-package :bin/skel)
(in-readtable :shell)

(defopt skc-help (print-help $cli))
(defopt skc-version (print-version $cli))
(defopt skc-level *log-level* (setq *log-level* (or $val :info)))

;; TODO 2023-10-13: almost there
(defopt skc-config
  (init-user-skelrc (when $val (parse-file-opt $val))))

(defcmd skc-edit
  (let ((file (or (when $args (pop $args)) (find-skelfile #P"."))))
    (cli/ed:run-emacsclient (namestring file))))
      
(defcmd skc-init
  (let ((file (when $args (pop $args)))
	(name (if (> $argc 1) (pop $args))))
    (handler-bind
	((sb-ext:file-exists 
	   #'(lambda (s)
	       (uiop:println (format nil "file already exists: ~A" (or file *default-skelfile*)))
	       (let ((f2 (read-line)))
		 (if (string= f2 "") 
		     (error s)
		     (use-value f2 s))))))
      (init-skelfile file name))))

(defcmd skc-describe
  (describe
   (find-skelfile 
    (if $args (pathname (car $args))
	#P".")
    :load t)))

(defcmd skc-inspect
  (sb-ext:enable-debugger)
  (inspect
   (find-skelfile
    (if $args (pathname (car $args))
	#P".")
    :load t)))

(defcmd skc-id
  (println (std:format-sxhash (obj/id:id (find-skelfile #P"." :load t)))))

(defun skc-show-case (sel)
  (std/string:string-case (sel :default (nyi!))
    (":id" (std:format-sxhash (obj/id:id (find-skelfile #P"." :load t))))
    (":config" (if (probe-file *user-skelrc*)
                   (describe (load-user-skelrc) t)
                   (describe *skel-user-config* nil)))
    (":vc" (sk-vc (find-skelfile #P"." :load t)))
    (":author" (sk-author (find-skelfile #P"." :load t)))
    (":scripts" (sk-scripts (find-skelfile #P"." :load t)))
    (":rules" (sk-rules (find-skelfile #P"." :load t)))
    (":description" (sk-description (find-skelfile #P"." :load t)))
    (":tags" (sk-tags (find-skelfile #P"." :load t)))
    (":docs" (sk-docs (find-skelfile #P"." :load t)))
    (":version" (sk-docs (find-skelfile #P"." :load t)))
    (":imports" (sk-imports (find-skelfile #P"." :load t)))
    (":license" (sk-license (find-skelfile #P"." :load t)))
    (":stash" (sk-stash (find-skelfile #P"." :load t)))
    (":store" (sk-store (find-skelfile #P"." :load t)))
    (":cache" (sk-cache (find-skelfile #P"." :load t)))))

(defcmd skc-show
  (if $args 
      (mapc (lambda (x) (when-let ((ret (skc-show-case x))) (println ret))) $args)
      (describe (find-skelfile #P"." :load t))))

(defcmd skc-push
  (case (sk-vc (find-skelfile #P"." :load t))
    (:hg (run-hg-command "push" $args t))))

(defcmd skc-pull
  (case (sk-vc (find-skelfile #P"." :load t))
    (:hg (run-hg-command "pull" (push "-u" $args) t))))

(defcmd skc-status
  (case (sk-vc (find-skelfile #P"." :load t))
    (:hg (progn
           (let ((proc (run-hg-command "status" nil :stream)))
             (copy-stream (process-output proc) *standard-output*)
             (finish-output))))
    (t (progn
            (let ((proc (run-git-command "status" nil :stream)))
              (copy-stream (process-output proc) *standard-output*)
              (finish-output))))))

(defcmd skc-make
  (if $args
      (mapc (lambda (rule) (debug! (sk-run (sk-find-rule rule (find-skelfile #P"." :load t))))) $args)
      (debug! (sk-run (aref (sk-rules (find-skelfile #P"." :load t)) 0)))))

(defcmd skc-run
  (if $args
      (mapc (lambda (script) (debug! (sk-run (sk-find-script script (find-skelfile #P"." :load t))))) $args)
      (required-argument :script)))

(defcmd skc-shell
  (if $args
      (nyi!)
      (let ((*no-exit* t))
        (in-package :skel)
        (use-package :std-user)
        ;; (sb-ext:enable-debugger)
        (require :sb-aclrepl)
        (init-skel-vars)
        (sb-impl::toplevel-repl nil))))

(define-cli $cli
  :name "skel"
  :version "0.1.1"
  :description "A hacker's project compiler and build tool."
  :thunk skc-describe
  :opts (make-opts 
	  (:name "help" :global t :description "print this message" 
	   :thunk skc-help)
	  (:name "version" :global t :description "print version" 
	   :thunk skc-version)
	  (:name "level" :global t :description "set log level (debug,info,trace,warn)"
	   :thunk skc-level)
	  (:name "config" :global t :description "set a custom skel user config" :kind file
	   :thunk skc-config)
	  (:name "input" :global t :description "input source" :kind string)
	  (:name "output" :global t :description "output target" :kind string))
  :cmds (make-cmds
	  (:name init
	   :description "initialize a skelfile in the current directory"
	   :opts (make-opts (:name "name" :description "project name" :kind string))
	   :thunk skc-init)
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
          (:name id
           :description "print the project id"
           :thunk skc-id)
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
	   :description "clone a remote project")
	  (:name commit
	   :description "commit changes to the project vc")
	  (:name edit
	   :description "edit a project file in emacs."
           :thunk skc-edit)
	  (:name shell
	   :description "open the sk-shell interpreter"
           :thunk skc-shell)))

(defun run ()
  (let ((*log-level* :info))
    (in-readtable :shell)
    (with-cli (opts cmds) $cli
      (load-skelrc)
      ;; TODO 2024-01-01: need to parse out CMD opts from args slot - they still there
      (do-opt (find-opt $cli "level"))
      (do-cmd $cli)
      (debug-opts $cli))))

(defmain ()
  (run)
  (sb-ext:exit :code 0))
