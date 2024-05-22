(defsystem :io
  :description "Lisp IO Interface"
  :depends-on (:cl-ppcre :std :obj :uring :sb-bsd-sockets)
  :version "0.1.0"
  :serial t
  :components ((:file "pkg")
               (:file "xsubseq")
               (:file "smart-buffer"))
  :in-order-to ((test-op (test-op "io/tests"))))

(defsystem :io/tests
  :depends-on (:rt :io :uring :sb-bsd-sockets)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :io)))
