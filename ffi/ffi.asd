;;; ~/comp/core/lisp/ffi/ffi.asd --- Ffi Sytem Definitions
(defsystem :ffi
  :depends-on 
  (:arrow :blake3 :btrfs :alsa
   :jack
   :cuda :evdev :glib :gstreamer
   :keyutils :rocksdb
   :sndfile :ssh2 :tree-sitter
   :uring :xkb :zstd :jpeg
   :chromaprint :blas :openssl :ffmpeg
   :syslog)
  :in-order-to ((test-op (test-op "obj/tests"))))

(defsystem :ffi/tests
  :depends-on 
  (:arrow/tests :blake3/tests :btrfs/tests
   :cuda/tests :evdev/tests :glib/tests :gstreamer/tests
   :keyutils/tests :rocksdb/tests :alsa/tests :sndfile/tests 
   :ssh2/tests :tree-sitter/tests :syslog/tests
   :uring/tests :xkb/tests :zstd/tests :chromaprint/tests
   :jack/tests :blas/tests :jpeg/tests :openssl/tests :ffmpeg/tests)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :ffi)))

