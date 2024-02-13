;;; homer.lisp --- homectl utility

;;; Code:
(defpackage :bin/homer
  (:nicknames :homer)
  (:use :cl :std :log :sxp :rdb :skel :packy :cli :obj/id)
  (:export :main :home-config))

(in-package :bin/homer)

(defvar *user* (sb-posix:getenv "USER"))
(defvar *user-homedir* (user-homedir-pathname))
(defvar *default-user-homerc* (merge-pathnames ".homerc" *user-homedir*))

(defclass home-config (sxp id)
  ((user :initform *user* :initarg :user :type string)
   (path :initform nil :initarg :path :type (or pathname null))
   (skel :initform (load-user-skelrc) :initarg :skel :type (or pathname sk-user-config))
   (krypt :initarg :krypt)
   (packy :initarg :packy :type (or pathname pk-user-config))
   (mail :initarg :mail)
   (shell :initarg :shell :type (or pathname shell-user-config))
   (editor :initarg :editor :type (or pathname editor-user-config))
   (wm :initarg :wm :type (or pathname wm-user-config))
   (browser :initarg :browser :type (or pathname browser-user-config))
   (paths :initarg :paths :type list)))

(defun find-homer-symbol (s)
  (find-symbol* (symbol-name s) :homer nil))

(defmethod load-ast ((self home-config))
  (with-slots (ast) self
    (if (formp ast)
        ;; ast is valid, modify object, set ast nil
        (progn
          (sb-int:doplist (k v) ast
            (when-let ((s (find-homer-symbol k)))
              (setf (slot-value self s) v))) ;; needs to be correct package
          (setf (ast self) nil)
          self)
        ;; invalid ast, signal error
        (error 'sxp-syntax-error))))

;; obj -> ast
(defmethod build-ast ((self sk-project) &key (nullp nil) (exclude '(ast id)))
  (setf (ast self)
         (unwrap-object self
                        :slots t
                        :methods nil
                        :nullp nullp
                        :exclude exclude)))

        
(defun load-homerc (&optional (file *default-user-homerc*))
  "Load a homerc configuration from FILE. Defaults to ~/.homerc."
  (let ((form (file-read-forms file)))
    (load-ast (make-instance 'home-config :ast form :path file :id (sxhash form)))))

(defopt homer-help (print-help $cli))
(defopt homer-version (print-version $cli))
(defopt homer-log-level (setq *log-level* (when $val :debug)))

(defcmd homer-show
  (describe (load-homerc)))


(define-cli $cli
  :name "homer"
  :version "0.1.0"
  :description "home manager"
  :opts (make-opts
          (:name level :global t :description "set the log level" :thunk homer-log-level)
          (:name help :global t :description "print help" :thunk homer-help)
          (:name version :global t :description "print version" :thunk homer-version))
  :cmds (make-cmds
          (:name show :thunk homer-show)))

(defun run ()
  (with-cli (opts cmds args) $cli
    (do-cmd $cli)
    (debug-opts $cli)))

(defmain ()
  (let ((*print-readably* t))
    (run)
    (sb-ext:exit :code 0)))
