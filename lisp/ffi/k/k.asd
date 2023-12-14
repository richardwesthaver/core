;;; k.asd --- K SYSTEMS

;; k for lisp.

;;; Commentary:

;; 

;;; Code:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-grovel))

(defpackage :k.sys
  (:use :cl :asdf :sb-grovel :sb-alien))

(in-package :k.sys)

(defsystem :k
  :description "ngn/k FFI"
  :depends-on (:sb-grovel :std)
  :components ((:file "pkg")
               (grovel-constants-file "constants"
                                      :package :k))
  :in-order-to ((test-op (test-op "k/tests"))))

(defsystem :k/tests
  :depends-on (:rt :k)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call :rt :do-tests :k)))
