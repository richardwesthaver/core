;;; core.lisp --- Core Multi-binary

;; Contains all core binaries - dispatches on argv[0].

;;; Code:
(defpackage :bin/core
  (:use :cl :std :sb-ext :cli :clap :log))

(in-package :bin/core)

(define-multi-main dispatch-core
  (let ((args (args)))
    (in-package :user)
    (if (and args (car args) (probe-file (car args)))
        (load (car args))
        (make-toplevel-init 
         :package :user 
         :userinit (lambda () (merge-homedir-pathnames ".corerc"))
         :default t)))
  (:skel (bin/skel::start-skel))
  (:homer (bin/homer::start-homer))
  (:mpk (bin/mpk:start-mpk)))
