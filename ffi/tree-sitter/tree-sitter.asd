;;; tree-sitter.asd --- TREE-SITTER SYSTEMS

;; TREE-SITTER for lisp.

;;; Code:
(defsystem :tree-sitter
  :depends-on (:std)
  :components ((:file "pkg")
               (:file "ffi")
               (:file "lang")
               (:file "api")
               (sb-grovel:grovel-constants-file "constants"
                                      :package :tree-sitter))
  :in-order-to ((test-op (test-op :tree-sitter/tests))))

(defsystem :tree-sitter/tests
  :depends-on (:rt :tree-sitter)
  :components ((:file "tests"))
  :perform (test-op (o c) (uiop:symbol-call :rt :do-tests :tree-sitter)))
