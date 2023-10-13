;;; btrfs.asd --- BTRFS SYSTEMS

;; BTRFS for lisp.

;;; Code:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-grovel))

(defpackage :btrfs.sys
  (:use :cl :asdf :sb-grovel :sb-alien))

(in-package :btrfs.sys)

(defsystem "btrfs"
  :version "0.1.0"
  :license (:file "LICENSE")
  :maintainer "ellis <ellis@rwest.io>"
  :homepage "https://nas-t.net"
  :bug-tracker "https://lab.rwest.io/comp/startup/nas-t/issues"
;;  :depends-on (:macs :sxp)
  :in-order-to ((test-op (test-op "btrfs/tests")))
  :components ((:module "btrfs"
                :components
                ((:file "btrfs")))))

(defsystem "btrfs/tests"
  :version "0.1.0"
  :license (:file "LICENSE")
  :maintainer "ellis <ellis@rwest.io>"
  :homepage "https://nas-t.net"
  :bug-tracker "https://lab.rwest.io/comp/startup/nas-t/issues"
  :depends-on (:btrfs :sb-rt :rt)
  :components ((:file "btrfs/tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:btrfs.tests '#:run-all-tests)))
