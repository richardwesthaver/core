(pushnew :prelude *features*)
(pushnew "PRELUDE" *modules* :test 'equal)
(defsystem :prelude
  :depends-on (:std :dat :cli :doc
               :io :gui :log :net 
               :nlp :obj :organ :packy
               :parse :pod :rdb :rt
               :skel :syn :xdb :alsa
               :app :rocksdb :btrfs :uring
               :tree-sitter :xkb :ssh2 :sndfile ;; magick
               :zstd :uring :blake3 :ublk
               :nuklear :aud :cry :krypt)
  :build-operation monolithic-compile-bundle-op
  :build-pathname "prelude")


