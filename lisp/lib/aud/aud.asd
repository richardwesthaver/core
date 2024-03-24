(defsystem :aud
  :description "High-level Audio API"
  :depends-on (:cl-ppcre :std :obj :dat :alsa :sndfile)
  :version "0.1.0"
  :serial t
  :components ((:file "pkg"))
  :in-order-to ((test-op (test-op "aud/tests"))))

(defsystem :aud/tests
  :depends-on (:rt :aud)
  :perform (test-op (o c) (symbol-call :rt :do-tests :aud)))
