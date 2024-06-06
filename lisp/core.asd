(pushnew :core *features*)
(defsystem :core
  :depends-on (:std :log :io :obj :net :cry :parse :syn :dat)
  :components ((:file "core"))
  :build-pathname "core"
  :build-operation monolithic-compile-bundle-op)

(defsystem :core/tests
  :depends-on (:std/tests :log/tests :rt/tests :cli/tests
               :dat/tests :rocksdb/tests :btrfs/tests :uring/tests
               :doc/tests :alsa/tests :nlp/tests :skel/tests
               :syn/tests :organ/tests :packy/tests :obj/tests 
               :tree-sitter/tests :xkb/tests :ssh2/tests :sndfile/tests
               :zstd/tests :uring/tests :blake3/tests :ublk/tests
               :parse/tests :pod/tests :rdb/tests :rt/tests
               :aud/tests :cry/tests :krypt/tests :io/tests
               :gui/tests :net/tests :vc/tests)
  :components ((:file "tests"))
  :build-pathname "tests"
  :build-operation monolithic-compile-bundle-op)

(defsystem :core/bench
  :depends-on (:core/tests)
  :components ((:file "bench"))
  :build-pathname "bench"
  :build-operation monolithic-compile-bundle-op)
