;;; lisp/core.lisp --- CC Core Lisp

;; Top-level namespaces accessible to Core lisps.

;;; Code:
(pkg:defpkg :core 
  (:use-reexport :std-lisp :log :io :obj :net :parse :dat :sb-ext :sb-debug :math)
  (:export #:app-config))


(in-package :core)

(define-lisp-package :core)

(defreadtable :core
  (:fuse :modern :std :shell :graph :q :math))

(eval-when (:load-toplevel)
  (pushnew :core *features*))

(defconfig app-config (service-config)
  ((logger :initarg :logger :type logger-config)
   (db :initarg :db :type db-config)
   (thread-pool :initarg :thread-pool :type thread-pool)
   (hook :initarg :hook :type hook)))

(pkg:defpkg :core/user
  (:nicknames :user)
  (:use :std-lisp :core)
  (:import-from :tree-sitter :load-tree-sitter :load-tree-sitter-alien :load-tree-sitter-c)
  (:import-from :cli/tools/sbcl :with-sbcl))

(in-package :user)

(eval-when (:compile-toplevel)
  (setq *default-package* "USER"))

(eval-when (:load-toplevel)
  (pushnew :user *features*))
