;;; cli.lisp --- Packy CLI Defs

;; 

;;; Code:
(in-package :skel/packy/cli)

(defvar *pk-target* nil)
(defopt pk-version (print-version *cli*))
(defopt pk-log-level 
  (setq log:*log-level* (if *arg* (if (stringp *arg*)
                                  (sb-int:keywordicate (string-upcase *arg*))
                                  *arg*)
                        :info)))
(defopt pk-target (setq *pk-target* *arg*))
(defcmd pk-show  ()
  (println (clap:active-opts *packy-cli*))
  (println (list :optc *optc* :argc *argc*
                 :opts *opts* :args *args*)))

(define-cli *packy-cli*
  :help t
  :name "packy"
  :version "0.1.0"
  :description "Universal Package Manager"
  :thunk pk-show
  :opts ((:name "level" :description "set the log level" :thunk level-opt)
         (:name "version" :description "print version" :thunk version-opt))
  :cmds ((:name show
          :opts ((:name "target" :thunk pk-target))
          :thunk pk-show)))

(defmain start-packy ()
  (with-cli (*packy-cli* :args (args))
    (do-cmd *packy-cli*)))
