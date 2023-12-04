;;; alpm.asd --- ALPM SYSTEMS

;; ALPM for lisp.

;;; Commentary:

;;; Code:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-grovel))

(defpackage :alpm.sys
  (:use :cl :asdf :sb-grovel :sb-alien))

(in-package :alpm.sys)

(defsystem :alpm
  :description "ALPM FFI"
  :depends-on (:sb-grovel :std)
  :components ((:file "pkg")
               (grovel-constants-file "constants"
                                      :package :alpm))
  :in-order-to ((test-op (test-op "alpm/tests"))))

(defsystem :alpm/tests
  :depends-on (:std/rt :alpm)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :alpm)))
