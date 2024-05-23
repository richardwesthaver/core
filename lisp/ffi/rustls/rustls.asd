;;; rustls.asd --- RUSTLS FFI bindings

;;

;;; Commentary:

;; 

;;; Code:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-grovel))

(defpackage :rustls.sys
  (:use :cl :asdf :sb-grovel :sb-alien))

(in-package :rustls.sys)

(defsystem :rustls
  :depends-on (:sb-grovel :std)
  :components ((:file "pkg")
               (grovel-constants-file "constants"
                                      :package :rustls))
  :in-order-to ((test-op (test-op "rustls/tests"))))

(defsystem :rustls/tests
  :depends-on (:rt :rustls)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call :rt :do-tests :rustls)))
