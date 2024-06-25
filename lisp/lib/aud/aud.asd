(defsystem :aud
  :description "High-level Audio API"
  :depends-on (:cl-ppcre :std :obj :dat :alsa :sndfile :net)
  :version "0.1.0"
  :serial t
  :components ((:file "pkg")
               (:file "mpd"))
  :in-order-to ((test-op (test-op "aud/tests"))))

(defsystem :aud/tests
  :depends-on (:rt :aud)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :aud)))
