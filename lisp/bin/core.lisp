;;; core.lisp --- Core Multi-binary

;; Contains all core binaries - dispatches on argv[0].

;;; Code:
(defpackage :bin/core
  (:use :cl :std :sb-ext :cli :log))

(in-package :bin/core)

(define-multi-main dispatch-core
    (sb-impl::toplevel-init)
    (:skel (bin/skel::start-skel))
    (:packy (bin/packy::start-packy))
    (:rdb (bin/rdb::start-rdb))
    (:organ (bin/organ::start-organ))
    (:homer (bin/homer::start-homer)))

