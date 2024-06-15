(pushnew :prelude *features*)
(defsystem :prelude
  :depends-on (:std :cli :log :dat
               :rocksdb :btrfs :uring :doc 
               :alsa :nlp :skel :syn
               :organ :packy :obj :net
               :tree-sitter :xkb :ssh2 :sndfile
               :zstd :uring :blake3 :ublk
               :parse :pod :rdb :gui
               :aud :cry :krypt :io)
  :build-operation monolithic-compile-bundle-op
  :build-pathname "prelude")
