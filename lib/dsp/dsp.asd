;;; dsp.asd --- Dsp Sytem Definitions
(defsystem :dsp
  :description "High-level DSP"
  :depends-on (:cl-ppcre :std :obj :dat :sndfile :net :gstreamer :log :io :jack :chromaprint :alsa :ffmpeg)
  :version "0.1.0"
  :serial t
  :components ((:file "pkg")
               (:file "var")
               (:file "proto")
               (:file "av")
               (:module "aud"
                :components 
                ((:file "pkg")
                 (:file "snd")
                 (:file "chroma")))
               (:module "vid"
                :components
                ((:file "pkg")))
               (:file "gst")
               (:file "dsp"))
  :in-order-to ((test-op (test-op "dsp/tests"))))

(defsystem :dsp/tests
  :depends-on (:rt :dsp)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :dsp)))
