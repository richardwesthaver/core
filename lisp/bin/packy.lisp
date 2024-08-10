(defpackage :bin/packy
  (:use :cl :std :sb-ext :cli :packy :clap :log)
  (:export :main))

(in-package :bin/packy)

;;; CLI
(defvar *pk-targets* nil)
(defopt pk-help (print-help *cli*))
(defopt pk-version (print-version *cli*))
(defopt pk-log-level (when *arg* (setq *log-level* :debug)))
(defopt pk-target (setq *pk-targets* *arg*))
(defcmd pk-show (print (list *optc* *argc* *opts* *args* *pk-targets*)))

(define-cli *cli*
  :name "packy"
  :version "0.1.0"
  :description "Universal Package Manager"
  :thunk 'pk-show
  :opts ((:name "level" :global t :description "set the log level" :thunk pk-log-level)
         (:name "help" :global t :description "print help" :thunk pk-help)
         (:name "version" :global t :description "print version" :thunk pk-version))
  :cmds ((:name show
          :opts (:name "target" :thunk pk-target)
          :thunk pk-show)))

(defun run ()
  (let ((*log-level* :info))
    (with-cli (opts cmds args) *cli*
      (do-cmd *cli*)
      (debug-opts *cli*))))

(defmain ()
  (let ((*print-readably* t))
    (run)
    (sb-ext:exit :code 0)))
