;;; cli.lisp --- CLI Top-level Packages

;; 

;;; Code:
(in-package :std-user)

(defpkg :cli
  (:use :cl :std :log)
  (:import-from :time :format-timestring :timestamp)
  #.`(:use-reexport ,@cli/int:*cli-packages*)
  (:export :sudop :with-sudo :pretty-log-message))

(defpkg :cli/tools
  (:nicknames :tools)
  (:use :cl :std)
  #.`(:use-reexport ,@cli/int:*cli-tool-packages*))

(defpkg :cli/clap
  (:nicknames :clap)
  #.`(:use-reexport ,@cli/int:*cli-clap-packages*))

(defpkg :cli-user 
  (:use :cl :std :cli :tools :clap))

(in-package :cli)
(pushnew :cli *features*)

;;; Sudo
(defun sudop () 
  "Return T if user appears to be root."
  (equal (namestring (user-homedir-pathname))
         (sb-unix::uid-homedir 0)))

(defmacro with-sudo (&body body)
  "Eval BODY with sudo privileges."
  `(progn
     (sb-ext:run-program "sudo" :input t :output t)
     ,@body))

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
