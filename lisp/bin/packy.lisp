(defpackage :bin/packy
  (:use :cl :std :sb-ext :cli :packy)
  (:export :main))

(in-package :bin/packy)

;;; CLI
(defopt pk-help (print-help $cli))
(defopt pk-version (print-version $cli))
(defopt pk-log-level (when $val (setq *log-level* :debug)))

(defcmd pk-show)

(define-cli $cli
  :name "packy"
  :version "0.1.0"
  :description "user home manager"
  :thunk pk-show
  :opts (make-opts
          (:name "level" :global t :description "set the log level" :thunk pk-log-level)
          (:name "help" :global t :description "print help" :thunk pk-help)
          (:name "version" :global t :description "print version" :thunk pk-version))
  :cmds (make-cmds
         (:name show :thunk pk-show)))

(defun run ()
  (let ((*log-level* :info))
    (with-cli (opts cmds args) $cli
      (do-cmd $cli)
      (debug-opts $cli))))

(defmain ()
  (let ((*print-readably* t))
    (run)
    (sb-ext:exit :code 0)))
