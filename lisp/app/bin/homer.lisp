;;; homer.lisp --- homectl utility

;;; Code:
(defpackage :bin/homer
  (:nicknames :homer)
  (:use :cl :std :log :sxp :rdb :skel :packy :cli)
  (:export :main :home-config))

(in-package :bin/homer)

(defvar *user* (sb-posix:getenv "USER"))
(defvar *user-homedir* (user-homedir-pathname))
(defvar *default-user-homerc* (merge-pathnames ".homerc" *user-homedir*))

(defclass home-config (sk-project)
  ((user :initform (sb-posix:getenv "USER") :initarg :user :type string)
   (skel :initform (load-skelrc) :initarg :skel :type (or pathname sk-user-config))
   (krypt :initarg :krypt)
   (packy :initarg :packy :type (or pathname pk-user-config))
   (mail :initarg :mail)
   (shell :initarg :shell :type (or pathname shell-user-config))
   (editor :initarg :editor :type (or pathname editor-user-config))
   (wm :initarg :wm :type (or pathname wm-user-config))
   (browser :initarg :browser :type (or pathname browser-user-config))
   (paths :initarg :paths :type list)))

(defun load-homerc (&optional file)
  "Load a homerc configuration from FILE. Defaults to ~/.homerc."
  (let ((form (file-read-forms (or file *default-user-homerc*))))
    (load-ast (make-instance 'home-config :ast form :id (sxhash form)))))

(defopt homer-help (print-help $cli))
(defopt homer-version (print-version $cli))
(defopt homer-log-level (setq *log-level* (when $val :debug)))

(defcmd homer-show
  (find-skelfile
   (user-homedir-pathname)
   :load t
   :name ".homerc"))

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
  (run)
  (sb-ext:exit :code 0))
