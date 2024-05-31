(pushnew :prelude *features*)
(defsystem :prelude
  :depends-on (:std :cli
               :rocksdb :btrfs :uring
               :doc
               :nlp :obj
               :skel :syn
               :xdb :alsa
               :organ :packy
               :tree-sitter :xkb :ssh2 :sndfile ;; magick
               :zstd :uring :blake3 :ublk
               :parse :pod :rdb :rt
               :nuklear :aud :cry :krypt
               :io :gui :log :dat :net)
  :components ((:file "prelude"))
  :build-operation monolithic-compile-bundle-op
  :build-pathname "prelude")
