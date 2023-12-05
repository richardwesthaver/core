;;; homer.lisp --- homectl utility

;;; Code:
(defpackage :cli/homer
  (:use :cl :std :std/log :std/fmt)
  (:export :main))

(in-package :cli/homer)

(defopt homer-help (print-help $cli))
(defopt homer-version (print-version $cli))
(defopt homer-log-level (setq *log-level* (when $val :debug)))

(defcmd homer-show)

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
