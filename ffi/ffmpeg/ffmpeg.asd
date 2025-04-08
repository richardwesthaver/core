;;; ffmpeg.asd --- FFMPEG FFI Sytem Definitions
(defsystem :ffmpeg
  :depends-on (:std :log)
  :components ((:file "pkg"))
  :in-order-to ((test-op (test-op :ffmpeg/tests))))
(defsystem :ffmpeg/tests
  :depends-on (:std :log :rt :ffmpeg)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call 'rt 'do-tests :ffmpeg)))
