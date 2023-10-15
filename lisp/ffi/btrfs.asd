;;; btrfs.asd --- BTRFS SYSTEMS

;; BTRFS for lisp.

;;; Code:
(defsystem "btrfs"
  :version "0.1.0"
  :license (:file "LICENSE")
  :maintainer "ellis <ellis@rwest.io>"
  :homepage "https://nas-t.net"
  :bug-tracker "https://lab.rwest.io/comp/core/issues"
  :depends-on (:macs :sxp)
  :in-order-to ((test-op (test-op "btrfs/tests")))
  :components ((:module "btrfs"
                :components
                ((:file "btrfs")))))

(defsystem "btrfs/tests"
  :version "0.1.0"
  :license (:file "LICENSE")
  :maintainer "ellis <ellis@rwest.io>"
  :homepage "https://nas-t.net"
  :bug-tracker "https://lab.rwest.io/comp/core/issues"
  :depends-on (:btrfs :rt)
  :components ((:file "btrfs/tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :btrfs)))
