;;; lisp/core.lisp --- CC Core Lisp

;; Top-level namespaces accessible to Core lisps.

;;; Code:
(pkg:defpkg :core 
  (:use-reexport :std-lisp :log :io :obj :net :parse :dat :sb-ext :sb-debug :math)
  (:export #:app-config))

(in-package :core)

(define-lisp-package :core)

(defreadtable :core
  (:fuse :modern :std :shell :graph :math))

(eval-when (:load-toplevel)
  (pushnew :core *features*))

(pkg:defpkg :core/user
  (:nicknames :user)
  (:use :std-lisp :core :cli)
  (:import-from :tree-sitter :load-tree-sitter :load-tree-sitter-c)
  (:import-from :tools :with-sbcl))

(in-package :user)

(eval-when (:compile-toplevel)
  (setq *default-package* "USER"))

(eval-when (:load-toplevel)
  (pushnew :user *features*))
