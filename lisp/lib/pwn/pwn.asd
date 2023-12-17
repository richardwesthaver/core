(defsystem :pwn
  :version "0.1.0"
  :maintainer "ellis <ellis@rwest.io>"
  :bug-tracker "https://vc.compiler.company/comp/core/issues"
  :depends-on (:sb-bsd-sockets :sb-capstone :cl-ppcre :std :obj :organ :dot :sxp :cli :net)
  :serial t
  :components ((:file "pkg")
               (:file "diz"))
  :in-order-to ((test-op (test-op "pwn/tests"))))

(defsystem :pwn/tests
  :depends-on (:rt :tests)
  :components ((:file "tests"))
  :perform ((test-op (o c) (uiop:symbol-call :rt :do-tests :pwn))))

