;;; core.lisp --- Core Multi-binary

;; Contains all core binaries - dispatches on argv[0].

;;; Code:
(defpackage :bin/core
  (:use :cl :std :sb-ext :cli :clap :log))

(in-package :bin/core)

(define-multi-main dispatch-core
  (progn (in-package :core-lisp)
         (default-toplevel-init))
  (:sbcl (sb-impl::toplevel-init))
  (:skel (bin/skel::start-skel))
  (:packy (bin/packy::start-packy))
  (:rdb (bin/rdb::start-rdb))
  (:vc (bin/vc:start-vc))
  (:pod (bin/pod::start-pod))
  (:organ (bin/organ::start-organ))
  (:homer (bin/homer::start-homer))
  (:mpk (bin/mpk:start-mpk))
  (:gen (bin/gen::start-gen)))
