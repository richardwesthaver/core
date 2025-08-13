;;; core.lisp --- Core Multi-binary

;; Contains all core binaries - dispatches on argv[0].

;;; Code:
(defpackage :bin/core
  (:use :cl :std :sb-ext :cli :clap :log))

(in-package :bin/core)

(define-multi-main dispatch-core
    (make-toplevel-init 
     :package :user 
     :userinit (lambda () (merge-homedir-pathnames ".corerc"))
     :default t)
  (:sbcl (sb-impl::toplevel-init))
  (:skel (bin/skel::start-skel))
  (:pod (bin/pod::start-pod))
  (:organ (bin/organ::start-organ))
  (:homer (bin/homer::start-homer))
  (:mpk (bin/mpk:start-mpk)))
