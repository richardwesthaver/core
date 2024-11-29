;;; cli.lisp --- CLI Top-level Packages

;; 

;;; Code:
(in-package :std-user)

(defpkg :cli/tools
  (:nicknames :tools)
  (:use :cl :std)
  (:use-reexport :cli/tools/term :cli/tools/tmux :cli/tools/cc
   :cli/tools/nvcc :cli/tools/pacman :cli/tools/systemd :cli/tools/cargo
   :cli/tools/sbcl :cli/tools/wg :cli/tools/ytdl :cli/tools/web))

(defpkg :cli
  (:use :cl :std :log)
  (:import-from :time :format-timestring :timestamp)
  (:use-reexport :cli/shell :cli/ansi :cli/prompt
   :cli/progress :cli/spark :cli/prompt :cli/ed
   :cli/env :cli/repl :cli/clap :cli/multi :cli/clap/obj)
  (:export :*sudo* :sudo-prompt :sudo? :when-sudo :if-sudo))

(defpkg :cli-user (:use :cl :std :cli))

(in-package :cli)

;;; Sudo prompt
(defvar *sudo* nil
  "Advise the Lisp system that we are allowed to use sudo for root access to shell commands. This is typically used in conjunction with SUDO-PROMPT.")

(defun %sudop (val) 
  (and (characterp val) (char= val #\y)))

(defprompt sudo
  :prompt "use sudo?" 
  :collection '(#\y #\n) 
  :default #\n 
  :reader #'read-char
  :test #'char=
  :hook #'%sudop)

(defun sudo? () (setq *sudo* (sudo-prompt)))

(defmacro if-sudo (then &optional else)
  `(if *sudo* ,then ,else))

(defmacro when-sudo (&body then)
  `(when *sudo* ,@then))

;;; Pretty Log Messages

(defclass pretty-log-message (simple-log-message) ())

(defmethod format-message (stream (message pretty-log-message))
  (let ((*standard-output* stream))
    (format stream log::*simple-log-message-formatter*
            (with-output-to-string (*standard-output*)
              (.sgr 48 5 7)
              (format-timestring *standard-output* 
                                 (timestamp message) 
                                 :format log::*log-timestamp-format*)
              (.sgr 0))
            (with-output-to-string (*standard-output*)
              (.sgr 48 5 7)
              (print (level message) *standard-output*)
              (.sgr 0))
            (with-output-to-string (*standard-output*)
              (.sgr 48 5 7)
              (print (log::tags message) *standard-output*)
              (.sgr 0))
            (format-message nil (log::content message)))))
