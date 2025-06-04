;;; lisp/user.lisp --- CC Lisp User Environments

;; Top-level namespaces accessible to user lisps.

;;; Code:
(pkg:defpkg :user (:use :std-lisp :std-user :cl-user :sb-ext))
(in-package :user)
(pushnew :user *features*)
(setq *default-package* "USER")
