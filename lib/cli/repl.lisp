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
