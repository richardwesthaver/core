;;; core.lisp --- Core Multi-binary

;; Contains all core binaries - dispatches on argv[0].

;;; Code:
(defpackage :bin/core
  (:use :cl :std :sb-ext :cli :clap :log))

(in-package :bin/core)

(define-multi-main dispatch-core
    (make-toplevel-init
     :package :user
     :userinit (constantly (xdg-config-file "corerc")))
  (:skel (bin/skel::start-skel))
  (:homer (bin/homer::start-homer))
  (:mpk (bin/mpk:start-mpk)))
