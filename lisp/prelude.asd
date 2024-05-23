(pushnew :prelude *features*)
(pushnew "PRELUDE" *modules* :test 'equal)

(defsystem :prelude
  :depends-on (:std :cli :doc
               :nlp :obj :organ :packy
               :parse :pod :rdb :rt
               :skel :syn :xdb :alsa
               :rocksdb :btrfs :uring
               :tree-sitter :xkb :ssh2 :sndfile ;; magick
               :zstd :uring :blake3 :ublk
               :nuklear :aud :cry :krypt
               :io :gui :log :dat :net)
  :build-operation monolithic-compile-bundle-op
  :build-pathname "prelude")


