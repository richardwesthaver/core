;;; cli.lisp --- CLI Top-level Packages

;; 

;;; Code:
(in-package :std-user)

(defpkg :cli
  (:use :cl :std :log)
  (:import-from :time :format-timestring :timestamp)
  (:use-reexport . #.cli-int:*cli-packages*)
  (:export :sudop :call-with-sudo :with-sudo :pretty-log-message :*sudo-output* :ensure-sudo))

(defpkg :cli/tools
  (:nicknames :tools)
  (:use :cl :std)
  (:use-reexport . #.cli-int:*cli-tool-packages*))

(defpkg :cli/clap
  (:nicknames :clap)
  (:prelude :clap*
   :defcmd :defopt
   :*argc* :*args* :*optc* :*opts* 
   :args :arg0
   :getopt :setopt
   :find-opt
   :*cli* :define-cli
   :defmain :with-cli
   :do-cmd :do-opt
   :load-package-cli
   :defcmd :defopt
   :defopts :argp
   :cmds :opts
   :help-opt :version-opt :level-opt :keep-ast-opt)
  (:use-reexport . #.cli-int:*cli-clap-packages*))

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

;;; DEFSYS Providers
(std/defsys::defprovider :cli (name &key package)
  `(clap:load-package-cli ,name . ,(when package '(:package package))))

;; (defprovider :keymap (name &key package))
