;;; organ.lisp --- Org-mode utility

;;

;;; Code:
(defpackage :bin/organ
  (:use :cl :organ :std :cli :log)
  (:export :main))

(in-package :bin/organ)

(defopt organ-help (print-help $cli))
(defopt organ-version (print-version $cli))
(defopt organ-log-level (setq *log-level* (if $val t :info)))

(defcmd organ-describe
  (if $args
      ;; TODO typed args
      (describe (org-parse :document (pathname (car $args))))
      (describe (org-parse :document #P"readme.org"))))

(defcmd organ-inspect
  (if $args
      ;; TODO typed args
      (inspect (org-parse :document (pathname (car $args))))
      (inspect (org-parse :document #P"readme.org"))))

(defcmd organ-show
  (if $args
      (print (org-parse-lines t (uiop:read-file-string (car $args))))
      (error! "missing file arg")))

(defcmd organ-parse
  (fmt-tree t (remove-if #'null (org-parse-lines :document (open (car $args)))) :layout :down))

(define-cli $cli
  :name "organ"
  :version "0.0.1"
  :description "org-mode toolbox"
  :thunk organ-describe
  :opts (make-opts 
	  (:name level :global t :description "set the log level" :thunk organ-log-level)
	  (:name help :global t :description "print help" :thunk organ-help)
	  (:name version :global t :description "print version" :thunk organ-version))
  :cmds (make-cmds 
	  (:name inspect 
           :description "inspect an org file"
           :opts (make-opts (:name input :description "path to org file" :kind file))
           :thunk organ-inspect)
          (:name show :opts nil :description "display local org info" :thunk organ-show)
          (:name describe :opts (make-opts (:name input :description "path to org file" :kind file))
           :description "describe local org info" :thunk organ-describe)
	  (:name parse 
	   :opts (make-opts (:name input :description "input source" :kind file) 
                   (:name output :description "compiler output" :kind string))
	   :thunk organ-parse)))

(defun run ()
  (let ((*log-level* :info))
    (with-cli (opts cmds args) $cli
      (do-cmd $cli)
      (debug-opts $cli))))

(defmain ()
  (run)
  (sb-ext:exit :code 0))
