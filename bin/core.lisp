;;; core.lisp --- Core Multi-binary

;; Contains all core binaries - dispatches on argv[0].

;;; Code:
(defpackage :bin/core
  (:use :cl :std :sb-ext :cli :clap :log))

(in-package :bin/core)

(add-hook '*init-hooks* 'init-xdg-dirs)

;; (add-hook '*init-hooks* 'std/os::init-xdg-logical-pathnames :append t)

(define-multi-main dispatch-core
    (make-toplevel-init
     :package :user
     :userinit (constantly (xdg-config-file "corerc")))
  (:skel (bin/skel::start-skel))
  (:homer (bin/homer::start-homer))
  (:mpk (bin/mpk:start-mpk)))
