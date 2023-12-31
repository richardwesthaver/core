;;; tree-sitter.asd --- TREE-SITTER SYSTEMS

;; TREE-SITTER for lisp.

;;; Code:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-grovel))

(defpackage :tree-sitter.sys
  (:use :cl :asdf :sb-grovel :sb-alien))

(in-package :tree-sitter.sys)

(defsystem :tree-sitter
  :depends-on (:sb-grovel :std)
  :components ((:file "pkg")
               (:file "ffi")
               (:file "lang")
               (:file "api")
               (grovel-constants-file "constants"
                                      :package :tree-sitter))
  :in-order-to ((test-op (test-op :tree-sitter/tests))))

(defsystem :tree-sitter/tests
  :depends-on (:rt :tree-sitter)
  :components ((:file "tests"))
  :perform (test-op (o c) (uiop:symbol-call :rt :do-tests :tree-sitter)))
