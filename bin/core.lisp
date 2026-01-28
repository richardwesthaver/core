;;; core.lisp --- Core Multi-binary

;; Contains all core binaries - dispatches on argv[0].

;;; Code:
(defpackage :bin/core
  (:use :cl :std :sb-ext :cli :clap :log))

(in-package :bin/core)

(define-multi-main dispatch-core
    (make-toplevel-init
     :package :user
     :userinit (lambda () (init :xdg) (xdg-config-file "corerc")))
  (:skel (skel/cli::start-skel))
  (:homer (skel/homer/cli::start-homer))
  (:mpk (mpk/cli::start-mpk)))
