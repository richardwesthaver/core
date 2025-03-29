;;; chromaprint.asd --- CHROMAPRINT SYSTEMS

;; CHROMAPRINT for lisp.

;;; Commentary:

;;; Code:
(defsystem :chromaprint
  :description "CHROMAPRINT C FFI"
  :depends-on (:std)
  :in-order-to ((test-op (test-op "chromaprint/tests")))
  :components ((:file "pkg"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :chromaprint)))

(defsystem "chromaprint/tests"
  :depends-on (:rt :chromaprint)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :chromaprint)))
