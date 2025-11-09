;;; sndfile.asd --- SNDFILE SYSTEMS

;; SNDFILE for lisp.

;;; Commentary:

;;; Code:
(defpackage :sndfile.sys
  (:use :cl :asdf :sb-grovel :sb-alien))

(in-package :sndfile.sys)

(defsystem "sndfile"
  :description "SNDFILE C FFI"
  :depends-on (:std)
  :in-order-to ((test-op (test-op "sndfile/tests")))
  :components ((:file "pkg")
               (grovel-constants-file "constants"
                                      :package :sndfile))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :sndfile)))

(defsystem "sndfile/tests"
  :depends-on (:rt :sndfile)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :sndfile)))
