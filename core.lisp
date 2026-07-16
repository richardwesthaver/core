;;; lisp/core.lisp --- CC Core Lisp

;; Top-level namespaces accessible to Core lisps.

;;; Code:
(in-package :std-user)

(defpkg :core
  (:use-reexport :std-lisp :log :io :obj :net :parse :dat :sb-ext :sb-debug :math
   :cli :skel :homer :mpk :krypt :packy :rdb :syn :cry :q :vc :box 
   :doc :dsp :pod :organ
   :rt)
  (:import-from :cli/main :define-multi-main)
  (:import-from :cli/shell :make-toplevel-init)
  (:export #:app-config #:dispatch-core))

(in-package :core)

(defreadtable :core (:fuse :modern :std :shell :graph :math :tempo :time :organ :tensor))

(define-lisp-package :core)

(pkg:defpkg :core/user
  (:nicknames :user)
  (:use :core-lisp)
  (:import-from :tools :with-sbcl))

(defconfig core-config (ast)
  (skel homer mpk krypt packy editor))

(defmethod make-config ((self (eql :core)) &rest args)
  (apply 'make-instance 'core-config args))

(defmethod load-config ((self (eql :core)) (from pathname) &key build)
  (let ((c (make-config :core)))
    (with-safe-io-syntax (:core)
      (read-ast c from)
      (load-ast c))
    (setf (ast c) nil)
    (if build (build c) c)))

(defmethod load-config ((self (eql :core)) (from list) &key build)
  (let ((c (make-config :core :ast from)))
    (with-safe-io-syntax (:core)
      (load-ast c))
    (setf (ast c) nil)
    (if build (build c) c)))
  
(defmethod find-config ((self (eql :core)) &key load)
  (let ((path (or (xdg-config-file :core) (xdg-config-file :init))))
    (if load (load-config :core path) path)))

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
      (setf *skel-user-config* skel
            *home-config* homer
            mpk::*mpk-user-config* mpk
            *krypt-user-config* krypt
            packy::*packy-config* packy
            *editor-config* editor))
  self)

(defun user-init-file () 
  (or (xdg-config-file "rc") (probe-file (merge-homedir-pathnames "init.lisp"))))

(defun sys-init-file () 
  (or (probe-file #p"/etc/rc") (probe-file #p"/etc/init.lisp")))

(define-multi-main dispatch-core
    (make-toplevel-init
     :package :user
     :userinit #'user-init-file
     :sysinit #'sys-init-file)
  (:skel (skel/cli::start-skel))
  (:homer (skel/homer/cli::start-homer))
  (:mpk (skel/mpk/cli::start-mpk)))

(defmethod init ((self (eql :core)) &key (readtable :core) (config (find-config :core)))
  (setq *readtable* (find-readtable readtable))
  (when config (load-config :core config :build t)))
