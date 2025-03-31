;;; alsa.asd --- ALSA SYSTEMS

;; ALSA for lisp.

;;; Commentary:

;;; Code:
(defsystem "alsa"
  :description "ALSA C FFI"
  :depends-on (:std)
  :in-order-to ((test-op (test-op "alsa/tests")))
  :components ((:file "pkg"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :alsa)))

(defsystem "alsa/tests"
  :depends-on (:rt :alsa)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :alsa)))
