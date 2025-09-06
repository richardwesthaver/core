;;; syslog.asd --- Linux Syslog FFI bindings

;;

;;; Commentary:

;; 

;;; Code:
(defpackage :syslog.sys
  (:use :cl :asdf :sb-grovel :sb-alien))

(in-package :syslog.sys)

(defsystem :syslog
  :depends-on (:sb-grovel :std)
  :components ((:file "pkg")
               (grovel-constants-file "constants"
                                      :package :syslog)
               (:file "syslog"))
  :in-order-to ((test-op (test-op "syslog/tests"))))

(defsystem :syslog/tests
  :depends-on (:rt :syslog)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call :rt :do-tests :syslog)))
