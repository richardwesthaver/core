;;; lisp/user.lisp --- CC Lisp User Environments

;; Top-level namespaces accessible to user lisps.

;;; Code:
(in-package :std-user)
(define-lisp-package :std)
(defpkg :user (:use :std-lisp :std-user))
