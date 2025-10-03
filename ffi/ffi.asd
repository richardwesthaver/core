;;; ~/comp/core/lisp/ffi/ffi.asd --- Ffi Sytem Definitions
(defsystem :ffi
  :depends-on (:sb-grovel
               :arrow :blake3 :btrfs :alsa
               :jack :matroska
               :cuda :evdev :glib :gstreamer
               :keyutils :rocksdb :rustls
               :sndfile :ssh2 :tree-sitter :ublk
               :uring :xkb :zstd :jpeg
               :chromaprint :blas :openssl :ffmpeg
               :syslog)
  :in-order-to ((test-op (test-op "obj/tests"))))

(defsystem :ffi/tests
  :depends-on (:arrow/tests :blake3/tests :btrfs/tests :matroska/tests
               :cuda/tests :evdev/tests :glib/tests :gstreamer/tests
               :keyutils/tests :rocksdb/tests :rustls/tests :alsa/tests
               :sndfile/tests :ssh2/tests :tree-sitter/tests :ublk/tests
               :uring/tests :xkb/tests :zstd/tests :chromaprint/tests
               :jack/tests :blas/tests :jpeg/tests :openssl/tests :wasmer/tests :ffmpeg/tests :syslog/tests)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :ffi)))

