;;; btrfs.asd --- BTRFS SYSTEMS

;; BTRFS for lisp.

;;; Code:
(defsystem "btrfs"
  :version "0.1.0"
  :license (:file "LICENSE")
  :maintainer "ellis <ellis@rwest.io>"
  :bug-tracker "https://lab.rwest.io/comp/core/issues"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:std :btrfs/pkg)
  :in-order-to ((test-op (test-op "btrfs/tests")))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :btrfs)))
  
