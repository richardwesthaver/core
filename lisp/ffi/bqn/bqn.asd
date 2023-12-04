;;; bqn.asd --- BQN SYSTEMS

;; BQN for lisp.

;;; Commentary:

;; you must build CBQN with 'make shared-o3' and place the resulting
;; .so in a known path.

;; see also: https://github.com/Detegr/cbqn-sys

;;; Code:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-grovel))

(defpackage :bqn.sys
  (:use :cl :asdf :sb-grovel :sb-alien))

(in-package :bqn.sys)

(defsystem :bqn
  :description "CBQN FFI"
  :depends-on (:sb-grovel :std)
  :components ((:file "pkg")
               (grovel-constants-file "constants"
                                      :package :bqn))
  :in-order-to ((test-op (test-op "bqn/tests"))))

(defsystem :bqn/tests
  :depends-on (:std/rt :bqn)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :bqn)))
