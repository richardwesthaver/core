;;; cli.lisp --- CLI Top-level Packages

;; 

;;; Code:
(in-package :std-user)

(defpkg :cli
  (:use :cl :std)
  (:use-reexport :cli/shell :cli/ansi :cli/prompt
   :cli/progress :cli/spark :cli/prompt :cli/ed
   :cli/env :cli/repl :cli/clap :cli/multi)
  (:export :*sudo* :sudo-prompt :sudo? :when-sudo :if-sudo :sudo-p))

(defpkg :cli-user (:use :cl :std :cli))

(in-package :cli)

(defun sudo-p ()
  "Return T if effective user is root."
  (zerop (parse-integer (with-output-to-string (str) (sb-ext:process-output (sb-ext:run-program "id" (list "-u") :search t :output str)) 0))))

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
