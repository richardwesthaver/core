;;; alsa.asd --- ALSA SYSTEMS

;; ALSA for lisp.

;;; Commentary:

;;; Code:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-grovel))

(defpackage :alsa.sys
  (:use :cl :asdf :sb-grovel :sb-alien))

(in-package :alsa.sys)

(defsystem "alsa"
  :description "ALSA C FFI"
  :depends-on (:sb-grovel :std)
  :in-order-to ((test-op (test-op "alsa/tests")))
  :components ((:file "pkg")
               (grovel-constants-file "constants"
                                      :package :alsa))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :alsa)))

(defsystem "alsa/tests"
  :depends-on (:rt :alsa)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :alsa)))
