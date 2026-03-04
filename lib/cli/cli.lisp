;;; cli.lisp --- CLI Top-level Packages

;; 

;;; Code:
(in-package :std-user)

(defpkg :cli
  (:use :std-lisp :log)
  (:use-reexport . #.(cons :cli/tools cli-int::*cli-packages*))
  (:import-from :time :format-string :timestamp)
  (:export :sudop :call-with-sudo :with-sudo :*sudo-output* :ensure-sudo))

(defpkg :cli-user 
  (:use :cl :std :cli :tools :clap))

(in-package :cli)
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

;;; DEFSYS Providers
(std/defsys::defprovider :cli (name &key package)
  (register-module :cli name `(clap:load-cli ,name ,@(when package `(,package)))))
  
(defmethod init ((self (eql :editor)) &rest args)
  ;; performs :TERM init internally (set-terminal)
  (setq cli/ed:*editor* (apply 'cli/linedit::make-editor args)))

(defmethod init ((self (eql :repl))
                 &key wrap (eof :quit)
                      history killring
                      acl)
  (when acl (require 'sb-aclrepl))
  (cli/linedit:install-repl :wrap-current wrap :eof-quits eof :history history :killring killring))

(defmethod init ((self (eql :term)) &key name (color t))
  (set-terminal name)
  (when color (setq std:*print-color* (capability :max-colors))))

(defmethod init ((self (eql :main))
                 &key (package *package*)
                      (userinit #'sb-impl::userinit-pathname)
                      (sysinit #'sb-impl::sysinit-pathname)
                      (default #'sb-impl::toplevel-init))
  (make-toplevel-init :package package
                      :userinit userinit
                      :sysinit sysinit
                      :default default))
