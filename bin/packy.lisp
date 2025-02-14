(defpackage :bin/packy
  (:use :cl :std :sb-ext :cli :packy :clap :log))

(in-package :bin/packy)

;;; CLI
(defvar *pk-target* nil)
(defopt pk-version (print-version *cli*))
(defopt pk-log-level 
  (setq *log-level* (if *arg* (if (stringp *arg*)
                                  (sb-int:keywordicate (string-upcase *arg*))
                                  *arg*)
                        :info)))
(defopt pk-target (setq *pk-target* *arg*))
(defcmd pk-show  ()
  (println (clap:active-opts *packy-cli*))
  (println (list :optc *optc* :argc *argc*
                 :opts *opts* :args *args* )))

(define-cli *packy-cli*
  :help t
  :name "packy"
  :version "0.1.0"
  :description "Universal Package Manager"
  :thunk pk-show
  :opts ((:name "level" :description "set the log level" :thunk pk-log-level)
         (:name "version" :description "print version" :thunk pk-version))
  :cmds ((:name show
          :opts ((:name "target" :thunk pk-target))
          :thunk pk-show)))

(defun run ()
  (with-cli (*packy-cli* :args (cli:args))
    (do-cmd *packy-cli*)))

(defmain start-packy ()
  (run))
    

