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
  :bug-tracker "https://lab.rwest.io/comp/core/issues"
  :depends-on (:sb-grovel :std)
  :components ((:file "pkg")
               (grovel-constants-file "constants"
                                      :package :btrfs))
  :in-order-to ((test-op (test-op "btrfs/tests"))))
  
(defsystem "btrfs/tests"
  :depends-on (:std/rt :btrfs)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :btrfs)))
