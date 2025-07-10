;;; cli.lisp --- CLI Top-level Packages

;; 

;;; Code:
(in-package :std-user)

(defpkg :cli
  (:use :cl :std :log)
  (:import-from :time :format-timestring :timestamp)
  #.`(:use-reexport ,@cli/int:*cli-packages*)
  (:export :sudop :call-with-sudo :with-sudo :pretty-log-message :*sudo-output* :ensure-sudo))

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

(in-readtable :shell)

;;; Sudo
(definline sudop () 
  "Return T if effective user appears to be root."
  (zerop (sb-posix:geteuid)))

(defvar *sudo-output* t)

(defun call-with-sudo (str &optional (output *sudo-output*))
  (sb-ext:run-program (find-exe "sudo") `("-S" ,@(split-sequence #\space str)) :input t :output output))

(defun ensure-sudo ()
  "Run sudo with input from *standard-input*, validating the credential cache
only."
  (unless (sudop) (sb-ext:run-program (find-exe "sudo") '("-v") :input t :output *sudo-output*)))
    
(defmacro with-sudo (&body body)
  "Eval BODY, a list of shell command strings, with sudo privileges."
  `(progn ,@(mapcar (lambda (x) `(call-with-sudo ,x)) body)))

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
