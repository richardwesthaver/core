;;; nuklear.asd --- NUKLEAR SYSTEMS

;; NUKLEAR for lisp.

;;; Code:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-grovel))

(defpackage :nuklear.sys
  (:use :cl :asdf :sb-grovel :sb-alien))

(in-package :nuklear.sys)

(defsystem "nuklear"
  :version "0.1.0"
  :license (:file "LICENSE")
  :maintainer "ellis <ellis@rwest.io>"
  :bug-tracker "https://vc.compiler.company/comp/core/issues"
  :depends-on (:sb-grovel :std)
  :components ((:file "pkg")
               (grovel-constants-file "constants"
                                      :package :nuklear))
  :in-order-to ((test-op (test-op "nuklear/tests"))))

(defsystem "nuklear/tests"
  :depends-on (:rt :nuklear)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :nuklear)))
