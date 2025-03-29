;;; cli.lisp --- Organ CLI

;; 

;;; Code:
(in-package :organ/cli)

;; (defopt organ-output (when *arg* (trace! (or *arg* "output.organ"))))
(defcmd organ-describe ()
  (if *args*
      ;; TODO typed args
      (describe (org-parse :document (pathname (car *args*))))
      (describe (org-parse :document #P"readme.org"))))

(defcmd organ-inspect ()
  (if *args*
      ;; TODO typed args
      (inspect (org-parse :document (pathname (car *args*))))
      (inspect (org-parse :document #P"readme.org"))))

(defcmd organ-show ()
  (if *args*
      (print (org-parse-lines t (uiop:read-file-string (car *args*))))
      (log:error! "missing file arg")))

(defcmd organ-parse ()
  (let ((input (if *args* (car *args*) #P"readme.org")))
    (describe (org-parse :document input))))

(define-cli *organ-cli*
  :help t
  :package :organ
  :name "organ"
  :version "0.0.1"
  :description "org-mode toolbox"
  :thunk organ-describe
  :opts ((:name "level" :description "set the log level" :thunk organ-log-level)
	 (:name "version" :description "print version" :thunk organ-version)
	 ;; (:name "output" :description "output file" :kind file :thunk organ-output)
	 )
  :cmds ((:name inspect 
	  :description "inspect an org file"
	  :thunk organ-inspect)
	 (:name show
	  :description "display local org info"
	  :thunk organ-show)
	 (:name describe
	  :description "describe local org info"
	  :thunk organ-describe)
	 (:name parse
	  :thunk organ-parse)))
