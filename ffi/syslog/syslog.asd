;;; syslog.asd --- Linux Syslog FFI bindings

;;

;;; Commentary:

;; 

;;; Code:
(defsystem :syslog
  :depends-on (:std)
  :components ((:file "pkg")
               (sb-grovel:grovel-constants-file "constants"
                                      :package :syslog)
               (:file "syslog"))
  :in-order-to ((test-op (test-op "syslog/tests"))))

(defsystem :syslog/tests
  :depends-on (:rt :syslog)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call :rt :do-tests :syslog)))
