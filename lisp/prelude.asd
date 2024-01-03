(defsystem :prelude
  :depends-on (:std :cli :dat :doc
               :gui :log :net :nlp
               :obj :organ :packy
               :parse :pod :rdb :rt
               :skel :syn :xdb
               :app :rocksdb :btrfs
               :tree-sitter :xkb :ssh2
               :zstd :uring :blake3)
  :build-operation monolithic-compile-bundle-op
  :build-pathname "prelude")
