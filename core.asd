(defsystem :core
  :depends-on 
  (:std :std :cli :log 
   :dat :rocksdb :btrfs :doc 
   :nlp :skel :syn
   :organ :packy :obj :net
   :tree-sitter :xkb :ssh2 :sndfile
   :zstd :uring :blake3 :gstreamer :q :dsp :math ;; :ublk
   :parse :pod :rdb :gui
   :cry :krypt :io :glib)
  :components ((:file "core"))
  :build-pathname "core-source"
  :build-operation monolithic-concatenate-source-op)

(defsystem :core/tests
  :depends-on (:rt :std/tests :log/tests :rt/tests :cli/tests
               :dat/tests :rocksdb/tests :btrfs/tests :uring/tests
               :doc/tests :nlp/tests :skel/tests
               :syn/tests :organ/tests :packy/tests :obj/tests 
               :tree-sitter/tests :xkb/tests :ssh2/tests :sndfile/tests
               :zstd/tests :uring/tests :blake3/tests :ublk/tests
               :parse/tests :pod/tests :rdb/tests :rt/tests
               :dsp/tests :cry/tests :krypt/tests :io/tests
               :gui/tests :net/tests :vc/tests :math/tests)
  :components ((:file "tests"))
  :build-pathname "tests"
  :build-operation monolithic-compile-bundle-op)

(defsystem :core/bench
  :depends-on (:std :rt :core)
  :components ((:module "bench"
                :components ((:file "pkg")
                             (:module "net"
                              :components
                              ((:file "lan-party")))
                             (:module "db"
                              :components
                              ((:file "log")
                               (:file "stress")
                               (:module "tpc-h"
                                :components
                                ((:file "pkg")
                                 (:file "dbgen")
                                 (:file "tpc-h")))))
                             (:module "dat"
                              :components
                              ((:file "json")))
                             (:module "trivial"
                              :components
                              ((:file "array")
                               (:file "hash")
                               (:file "simd")
                               (:file "alien")))
                             (:file "bench"))))
  :build-pathname "bench"
  :build-operation monolithic-compile-bundle-op)

(defsystem :core/lib
  :depends-on (:cli :log :dat :doc
               :nlp :skel :syn :organ
               :packy :obj :net :io
               :parse :pod :rdb :rt
               :dsp :cry :krypt :gui))

(defsystem :core/ffi
  :depends-on (:blake3 :btrfs :keyutils :rocksdb 
               :rustls :sndfile :ssh2 :tree-sitter 
               :ublk :uring :xkb :zstd))
