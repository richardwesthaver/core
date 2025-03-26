;;; ~/comp/core/lisp/ffi/ffi.asd --- Ffi Sytem Definitions
(defsystem :ffi
  :depends-on (:sb-grovel
               :arrow :blake3 :btrfs :alsa
               :jack
               :cuda :evdev :glib :gstreamer
               :keyutils :readline :rocksdb :rustls
               :sndfile :ssh2 :tree-sitter :ublk
               :uring :xkb :zstd :jpeg)
  :in-order-to ((test-op (test-op "obj/tests"))))

(defsystem :ffi/tests
  :depends-on (:arrow/tests :blake3/tests :btrfs/tests 
               :cuda/tests :evdev/tests :glib/tests :gstreamer/tests
               :keyutils/tests :readline/tests :rocksdb/tests :rustls/tests
               :sndfile/tests :ssh2/tests :tree-sitter/tests :ublk
               :uring/tests :xkb/tests :zstd/tests)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :ffi)))

