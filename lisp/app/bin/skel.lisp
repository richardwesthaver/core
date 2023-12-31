;;; Code:
(uiop:define-package :bin/skel
  (:use :cl :std :cli :skel :log)
  (:export :main))

(in-package :bin/skel)

(defopt skc-help (print-help $cli))
(defopt skc-version (print-version $cli))
(defopt skc-log (setq *log-level* (if $val :debug nil)))
;; TODO 2023-10-13: almost there
(defopt skc-config (init-skel-user-config (parse-file-opt $val)))

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
    (inspect
     (find-skelfile
      (if $args (pathname (car $args))
	  #P".")
      :load t)))

(defcmd skc-show
     (if $args 
         (find-skelfile (pathname (car $args)) :load t)
         (find-skelfile #P"." :load t)))

(defcmd skc-push
  (case
      (sk-vc
       (find-skelfile
	(if $args (pathname (car $args))
	    #P".")
	:load t))
    (:hg (run-hg-command "push"))))

(defcmd skc-make
  (if $args
      (debug! (sk-rules (find-skelfile (car $args) :load t)))
      (debug! (sk-rules (find-skelfile #P"." :load t)))))

(define-cli $cli
  :name "skel"
  :version "0.1.1"
  :description "A hacker's project compiler and build tool."
  :thunk skc-describe
  :opts (make-opts 
	  (:name help :global t :description "print this message" 
	   :thunk skc-help)
	  (:name version :global t :description "print version" 
	   :thunk skc-version)
	  (:name log :global t :description "set log level (debug,info,trace,warn)"
	   :thunk skc-log)
	  (:name config :global t :description "set a custom skel user config" :kind file
	   :thunk skc-config) ;; :kind?
	  (:name input :description "input source" :kind string)
	  (:name output :description "output target" :kind string))
  :cmds (make-cmds
	  (:name init
	   :description "initialize a skelfile in the current directory"
	   :opts (make-opts (:name name :description "project name" :kind string))
	   :thunk skc-init)
	  (:name show
	   :description "describe the project skelfile"
	   :opts (make-opts 
                   (:name file :description "path to skelfile" :kind file)
                   (:name user :description "print user configuration")
                   (:name system :description "print system configuration"))
	   :thunk skc-describe)
	  (:name inspect
	   :description "inspect the project skelfile"
	   :opts (make-opts (:name file :description "path to skelfile" :kind file))
	   :thunk skc-inspect)
	  (:name make
	   :description "build project targets"
	   :opts (make-opts (:name target :description "target to build" :kind string))
	   :thunk skc-make)
	  (:name run
	   :description "run a script or command")
	  (:name push
	   :description "push the current project upstream"
	   :thunk skc-push)
	  (:name pull
	   :description "pull the current project from remote")
	  (:name clone
	   :description "clone a remote project")
	  (:name commit
	   :description "commit changes to the project vc")
	  (:name edit
	   :description "edit a project file")
	  (:name shell
	   :description "open the sk-shell interpreter")))

(defun run ()
  (let ((*log-level* nil)
	(*skel-user-config* (init-skel-user-config)))
    (in-readtable :std)
    (with-cli () $cli
      (init-skel-vars)
      ;; TODO 2024-01-01: need to parse out CMD opts from args slot - they still there
      (do-cmd $cli)
      (debug-opts $cli))))

(defmain ()
  (run)
  (sb-ext:exit :code 0))
