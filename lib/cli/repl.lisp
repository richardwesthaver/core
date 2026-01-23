;;; lib/cli/repl.lisp --- REPL utils

;;; Code:
(in-package :cli/repl)

;;; Config
(defconfig repl-config (ast) 
  (sysinit userinit package))

;;; Toplevel
(defun make-toplevel-init (&key (package *package*)
                                (userinit #'sb-impl::userinit-pathname)
                                (sysinit #'sb-impl::sysinit-pathname)
                                (default #'sb-impl::toplevel-init))
  "Default toplevel initializer - wraps SBCL init when DEFAULT is T."
  (setq *package* (find-package package)
        sb-ext:*userinit-pathname-function* userinit
        sb-ext:*sysinit-pathname-function* sysinit)
  (when default (funcall default)))

;;; Init
(defmethod init ((self (eql :editor)) &rest args) 
  ;; performs :TERM init internally (set-terminal)
  (setq cli/linedit::*editor* (apply 'cli/linedit::make-editor args)))
  
(defmethod init ((self (eql :repl))
                 &key wrap (eof :quit)
                      history killring
                      acl)
  (when acl (require 'sb-aclrepl))
  (cli/linedit:install-repl :wrap-current wrap :eof-quits eof :history history :killring killring))

(defmethod init ((self (eql :term)) &key name (color t))
  (set-terminal name)
  (when color (setq std:*print-color* (capability :max-colors))))

(defmethod init ((self (eql :main))
                 &key (package *package*)
                      (userinit #'sb-impl::userinit-pathname)
                      (sysinit #'sb-impl::sysinit-pathname)
                      (default #'sb-impl::toplevel-init))
  (make-toplevel-init :package package
                      :userinit userinit
                      :sysinit sysinit
                      :default default))
