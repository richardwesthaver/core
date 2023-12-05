;;; organ.lisp --- Org-mode utility

;;; Code:
(defpackage :cli/organ
  (:use :cl :organ :std :std/fmt)
  (:export :main))

(in-package :cli/organ)

(defopt organ-help (print-help $cli))
(defopt organ-version (print-version $cli))
(defopt organ-log-level (setq *log-level* (if $val :debug nil)))

(defcmd organ-inspect (inspect (read-org-file (car $args))))

(defcmd organ-show
(fmt-tree t 
	  (mapcar (lambda (x) `(,(car x) ,(cddr x)))
		  (remove-if-not (lambda (x) (equal (cadr x) (symb 'headline))) 
				 (org-parse-lines (read-org-file (open (car $args))))))
	  :layout :down))

(defcmd organ-parse
  (fmt-tree t (remove-if #'null (org-parse-lines (read-org-file (open (car $args))))) :layout :down))

(define-cli $cli
  :name "organ"
  :version "0.0.1"
  :description "org-mode toolbox"
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
	  (:name parse 
	   :opts (make-opts (:name input :description "input source" :kind file) 
                   (:name output :description "compiler output" :kind string))
	   :thunk organ-parse)))

(defun run ()
  (with-cli (opts cmds args) $cli
    (do-cmd $cli)
    (debug-opts $cli)))

(defmain ()
  (run)
  (sb-ext:exit :code 0))


