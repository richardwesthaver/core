;;; btrfs.asd --- BTRFS SYSTEMS

;; BTRFS for lisp.

;;; Code:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-grovel))

(defpackage :btrfs.sys
  (:use :cl :asdf :sb-grovel :sb-alien))

(in-package :btrfs.sys)

(defsystem "btrfs"
  :depends-on (:sb-grovel :std)
  :components ((:file "pkg")
               (grovel-constants-file "constants"
                                      :package :btrfs)
               (:file "util" :depends-on ("pkg" "constants")))
  :in-order-to ((test-op (test-op "btrfs/tests"))))
  
(defsystem "btrfs/tests"
  :depends-on (:rt :btrfs)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :btrfs)))
