;;; bcachefs.asd --- BCACHEFS SYSTEMS

;; BCACHEFS ioctl wrappers and types for Lisp.

;;; Code:
(defsystem "bcachefs"
  :depends-on (:std)
  :components ((:file "pkg"))
  :in-order-to ((test-op (test-op "bcachefs/tests"))))
  
(defsystem "bcachefs/tests"
  :depends-on (:rt :bcachefs)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :bcachefs)))
