;;; lib/cli/repl.lisp --- REPL utils

;;; Code:
(in-package :cli/repl)

;;;_* Config
(defconfig repl-config (ast) 
  (sysinit userinit package))

;;;_. Toplevel
(defun make-toplevel-init (&key (package *package*)
                                (userinit #'sb-impl::userinit-pathname)
                                (sysinit #'sb-impl::sysinit-pathname)
                                (default #'sb-impl::toplevel-init))
  "Default toplevel initializer - wraps SBCL init when DEFAULT is T."
  (setq *package* (find-package package)
        sb-ext:*userinit-pathname-function* userinit
        sb-ext:*sysinit-pathname-function* sysinit)
  (when default (funcall default)))

;;;_* Init
(defmethod init ((self (eql :repl)) 
                 &key wrap (eof :quit)
                      history killring)
  (cli/linedit:install-repl :wrap-current wrap :eof-quits eof :history history :killring killring))

(defmethod init ((self (eql :main)) 
                 &key (package *package*)
                      (userinit #'sb-impl::userinit-pathname)
                      (sysinit #'sb-impl::sysinit-pathname)
                      (default #'sb-impl::toplevel-init))
  (make-toplevel-init :package package
                      :userinit userinit
                      :sysinit sysinit
                      :default default))
