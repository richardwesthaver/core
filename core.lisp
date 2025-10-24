;;; lisp/core.lisp --- CC Core Lisp

;; Top-level namespaces accessible to Core lisps.

;;; Code:
(pkg:defpkg :core 
  (:use-reexport :std-lisp :log :io :obj :net :cry :parse :dat :sb-ext :sb-debug :math)
  (:export
   #:core-app-config))

(in-package :core)

(define-lisp-package :core)

(defreadtable :core
  (:merge :modern :std :shell :graph :tensor :q))

(eval-when (:load-toplevel)
  (pushnew :core *features*))

(defconfig core-app-config (service-config)
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
