;;; ffi/mpv/mpv.asd --- MPV Sytem Definitions
(defsystem :mpv
  :depends-on (:std :log)
  :components ((:file "pkg"))
  :in-order-to ((test-op (test-op "mpv/tests"))))

(defsystem :mpv/tests
  :depends-on (:std :log :rt :mpv)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :mpv)))
