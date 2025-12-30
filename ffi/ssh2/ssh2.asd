;;; ssh2.asd --- SSH2 FFI bindings

;;

;;; Commentary:

;; 

;;; Code:
(defsystem :ssh2
  :depends-on (:std)
  :components ((:file "pkg")
               (sb-grovel:grovel-constants-file "constants"
                                      :package :ssh2)
               (:file "ssh2"))
  :in-order-to ((test-op (test-op "ssh2/tests"))))

(defsystem :ssh2/tests
  :depends-on (:rt :ssh2)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call :rt :do-tests :ssh2)))
