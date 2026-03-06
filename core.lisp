;;; lisp/core.lisp --- CC Core Lisp

;; Top-level namespaces accessible to Core lisps.

;;; Code:
(pkg:defpkg :core 
  (:use-reexport :std-lisp :log :io :obj :net :parse :dat :sb-ext :sb-debug :math :ast)
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

(defconfig core-config (ast)
  (skel homer mpk krypt packy editor))

(defmethod make-config ((self (eql :core)) &rest args)
  (apply 'make-instance 'core-config args))

(defmethod load-config ((self (eql :core)) (from pathname) &key)
  (let ((c (make-config :core)))
    (with-safe-io-syntax (:core)
      (read-ast c from)
      (load-ast c))
    (setf (ast c) nil)
    c))

(defmethod load-ast ((self core-config))
  (with-slots (ast) self
    (if (formp ast)
        (progn
          (sb-int:doplist (k v) ast
            (when-let ((s (find-symbol (symbol-name k) :core))) ;; needs to be correct package
              (setf v (apply 'make-config k v))
              (setf (slot-value self s) v)))
          (unless *keep-ast* (setf (ast self) nil))
          self)
        ;; invalid ast, signal error
        (error 'syntax-error))))

(defmethod build ((self core-config) &key)
  (with-slots (skel homer mpk krypt packy editor) self
      (setf skel:*skel-user-config* skel
            homer:*home-config* homer
            mpk::*mpk-user-config* mpk
            krypt:*krypt-user-config* krypt
            packy::*packy-config* packy
            cli/ed:*editor-config* editor))
  self)

(define-multi-main dispatch-core
    (make-toplevel-init
     :package :user
     :userinit (lambda () (init :xdg) (xdg-config-file "corerc")))
  (:skel (skel/cli::start-skel))
  (:homer (skel/homer/cli::start-homer))
  (:mpk (skel/mpk/cli::start-mpk)))
