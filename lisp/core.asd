(pushnew :core *features*)
(defsystem :core
  :depends-on (:std :log :io :obj :net :cry :parse :syn :dat)
  :components ((:file "core"))
  :build-pathname "core"
  :build-operation monolithic-compile-bundle-op)

(defsystem :core/tests
  :depends-on (:rt :std/tests :log/tests :rt/tests :cli/tests
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
  :depends-on (:std :core/tests :rt :obj :dat)
  :components ((:module "bench"
                :components ((:file "pkg")
                             (:module "db"
                              :components
                              ((:file "tpc-h")))
                             (:module "dat"
                              :components
                              ((:file "json")))
                             (:module "trivial"
                              :components
                              ((:file "array")
                               (:file "hash")
                               (:file "simd")
                               (:file "alien"))))))
  :build-pathname "bench"
  :build-operation monolithic-compile-bundle-op)

(pushnew :lib *features*)
(defsystem :core/lib
  :depends-on (:cli :log :dat :doc
               :nlp :skel :syn :organ
               :packy :obj :net :io
               :parse :pod :rdb :rt
               :aud :cry :krypt :gui)
  :build-operation monolithic-compile-bundle-op
  :build-pathname "lib")

(defsystem :core/ffi
  :depends-on (:alsa :blake3 :btrfs :keyutils
               :keyutils :readline :rocksdb :rustls
               :sndfile :ssh2 :tree-sitter :ublk
               :uring :xkb :zstd)
  :build-operation monolithic-compile-bundle-op
  :build-pathname "ffi")
