;;; sndfile.asd --- SNDFILE SYSTEMS

;; SNDFILE for lisp.

;;; Commentary:

;;; Code:
(defsystem "sndfile"
  :description "SNDFILE C FFI"
  :depends-on (:std)
  :in-order-to ((test-op (test-op "sndfile/tests")))
  :components ((:file "pkg")
               (sb-grovel:grovel-constants-file "constants"
                                      :package :sndfile))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :sndfile)))

(defsystem "sndfile/tests"
  :depends-on (:rt :sndfile)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :sndfile)))
