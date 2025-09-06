;;; lisp/core.lisp --- CC Core Lisp

;; Top-level namespaces accessible to Core lisps.

;;; Code:
(pkg:defpkg :core 
  (:use-reexport :std-lisp :log :io :obj :net :cry :parse :dat :sb-ext :sb-debug :math))
(in-package :core)
(pushnew :core *features*)

(defreadtable :core
  (:merge :modern :std :shell :graph :tensor :q))

(define-lisp-package :core)

(pkg:defpkg :core/user 
  (:nicknames :user)
  (:use :std-lisp :core)
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
