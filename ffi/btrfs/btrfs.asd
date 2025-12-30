;;; btrfs.asd --- BTRFS SYSTEMS

;; BTRFS for lisp.

;;; Code:
(defsystem "btrfs"
  :depends-on (:std)
  :components ((:file "pkg")
               (sb-grovel:grovel-constants-file "constants"
                                      :package :btrfs)
               (:file "util" :depends-on ("pkg" "constants")))
  :in-order-to ((test-op (test-op "btrfs/tests"))))
  
(defsystem "btrfs/tests"
  :depends-on (:rt :btrfs)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :btrfs)))
