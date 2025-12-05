(defsystem :core
  :depends-on 
  (:std :cli :log :obj
   :net :nlp :dat :doc
   :q :dsp :math :parse 
   :rdb :cry :io :pod
   :box :vc :syn
   :rt)
  :components ((:file "core"))
  :build-pathname "core-source"
  :build-operation monolithic-concatenate-source-op
  :in-order-to ((test-op (test-op "core/tests"))))

(defsystem :core/tests
  :depends-on (:core :std/tests :log/tests :rt/tests
               :dat/tests :rocksdb/tests :btrfs/tests :uring/tests
               :doc/tests :nlp/tests :skel/tests :box/tests
               :syn/tests :organ/tests :obj/tests :math/tests
               :tree-sitter/tests :xkb/tests :ssh2/tests :sndfile/tests
               :zstd/tests :uring/tests :blake3/tests
               :parse/tests :pod/tests :rt/tests :rdb/tests
               :dsp/tests :cry/tests :io/tests :net/tests 
               :vc/tests :cli/tests :q/tests)
  :perform (test-op (o c) (symbol-call :rt :run-all-tests)))

(defsystem :core/bench
  :depends-on (:core)
  :components ((:module "bench"
                :components ((:file "pkg")
                             (:file "lan-party")
                             (:module "tpc-h"
                              :components
                              ((:file "pkg")
                               (:file "dbgen")
                               (:file "tpc-h")))
                             (:file "bench"))))
  :build-pathname "bench"
  :build-operation monolithic-compile-bundle-op)
