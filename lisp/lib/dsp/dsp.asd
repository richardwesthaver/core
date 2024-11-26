(defsystem :dsp
  :description "High-level DSP"
  :depends-on (:cl-ppcre :std :obj :dat :alsa :sndfile :net :gstreamer :log :io)
  :version "0.1.0"
  :serial t
  :components ((:file "pkg"))
  :in-order-to ((test-op (test-op "dsp/tests"))))

(defsystem :dsp/tests
  :depends-on (:rt :dsp)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :aud)))
