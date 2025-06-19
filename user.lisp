;;; lisp/user.lisp --- CC Lisp User Environments

;; Top-level namespaces accessible to user lisps.

;;; Code:
(pkg:defpkg :user 
  (:use :cl :cl-user :std :std-user :core)
  (:import-from :tree-sitter :load-tree-sitter :load-tree-sitter-alien :load-tree-sitter-c)
  (:import-from :cli/tools/sbcl :with-sbcl))
(in-package :user)

(defpkg lib/prelude)
(defpkg ffi/prelude)
(defpkg prelude)

(eval-when (:compile-toplevel)
  (setq *default-package* "USER"))

(eval-when (:load-toplevel)
  (pushnew :user *features*))
