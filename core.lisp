;;; lisp/core.lisp --- CC Core Lisp

;; Top-level namespaces accessible to Core lisps.

;;; Code:
(pkg:defpkg :core 
  (:use-reexport :std-lisp :log :io :obj :net :parse :dat :sb-ext :sb-debug :math)
  (:import-from :cli/main :define-multi-main)
  (:import-from :cli/repl :make-toplevel-init)
  (:export #:app-config #:dispatch-core))

(in-package :core)

(define-lisp-package :core)

(defreadtable :core
  (:fuse :modern :std :shell :graph :math))

(pkg:defpkg :core/user
  (:nicknames :user)
  (:use :std-lisp :core :cli)
  (:import-from :tree-sitter :load-tree-sitter :load-tree-sitter-c)
  (:import-from :tools :with-sbcl))

(eval-when (:compile-toplevel)
  (setq *default-package* "USER"))

(define-multi-main dispatch-core
    (make-toplevel-init
     :package :user
     :userinit (lambda () (init :xdg) (xdg-config-file "corerc")))
  (:skel (skel/cli::start-skel))
  (:homer (skel/homer/cli::start-homer))
  (:mpk (skel/mpk/cli::start-mpk)))
