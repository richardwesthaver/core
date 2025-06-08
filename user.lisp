;;; lisp/user.lisp --- CC Lisp User Environments

;; Top-level namespaces accessible to user lisps.

;;; Code:
(pkg:defpkg :user 
  (:use :cl :cl-user :std :std-user :core)
  (:import-from :tree-sitter :load-tree-sitter :load-tree-sitter-alien :load-tree-sitter-c)
  (:import-from :cli/tools/sbcl :with-sbcl))
(in-package :user)
(pushnew :user *features*)
(setq *default-package* "USER")

(defpkg lib/prelude)
(defpkg ffi/prelude)
(defpkg prelude)
