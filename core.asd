(defsystem :core
  :depends-on 
  (:std :cli :log :obj
   :net :nlp :dat :doc
   :q :dsp :math :parse 
   :rdb :cry :io)
  :components ((:file "core"))
  :build-pathname "core-source"
  :build-operation monolithic-concatenate-source-op
  :in-order-to ((test-op (test-op "core/tests"))))

(defsystem :core/user
  :depends-on 
  (:core :pod :box :gui
   :web :vc :syn :rt)
  :components ((:file "user"))
  :build-operation monolithic-compile-bundle-op
  :build-pathname "user-source")

(defsystem :core/tests
  :depends-on (:rt :std/tests :log/tests :rt/tests :cli/tests
               :dat/tests :rocksdb/tests :btrfs/tests :uring/tests
               :doc/tests :nlp/tests :skel/tests :box/tests
               :syn/tests :organ/tests :packy/tests :obj/tests 
               :tree-sitter/tests :xkb/tests :ssh2/tests :sndfile/tests
               :zstd/tests :uring/tests :blake3/tests :ublk/tests
               :parse/tests :pod/tests :rt/tests :rdb/tests
               :dsp/tests :cry/tests :krypt/tests :io/tests
               :gui/tests :net/tests :vc/tests :math/tests)
  :components ((:file "tests"))
  :build-pathname "tests"
  :build-operation monolithic-compile-bundle-op
  :perform (test-op (o c) (symbol-call :rt :run-all-tests)))

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
