;;; core.lisp --- Core Multi-binary

;; Contains all core binaries - dispatches on argv[0].

;;; Code:
(defpackage :bin/core
  (:use :cl :std :sb-ext :cli :log))

(in-package :bin/core)

(define-multi-main dispatch-core
  (progn (in-package :core-lisp)
         (use-package '(:cl-user :sb-debug :sb-ext :std-user))
         (sb-impl::toplevel-init))
  (:sbcl (sb-impl::toplevel-init))
  (:skel (bin/skel::start-skel))
  (:packy (bin/packy::start-packy))
  (:rdb (bin/rdb::start-rdb))
  (:vc (bin/vc::start-vc))
  #+virt
  (:pod (bin/pod::start-pod))
  (:organ (bin/organ::start-organ))
  (:homer (bin/homer::start-homer))
  #+x11 
  (:swm (bin/swm::start-swm))
  (:gen (bin/gen::start-gen)))

