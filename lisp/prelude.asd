(defsystem :prelude
  :depends-on (:std :dat :cli :doc
               :io :gui :log :net 
               :nlp :obj :organ :packy
               :parse :pod :rdb :rt
               :skel :syn :xdb :alsa
               :app :rocksdb :btrfs :uring
               :tree-sitter :xkb :ssh2 :sndfile ;; magick
               :zstd :uring :blake3 :ublk
               :nuklear :aud)
               
  :build-operation monolithic-compile-bundle-op
  :build-pathname "prelude")
