(pushnew :prelude *features*)
(defsystem :prelude
  :depends-on (:std :cli :log :dat
               :rocksdb :btrfs :doc 
               :alsa :nlp :skel :syn
               :organ :packy :obj :net
               :tree-sitter :xkb :ssh2 :sndfile
               :zstd :uring :blake3 ;; :ublk
               :parse :pod :rdb :gui
               :cry :krypt :io :glib 
               :gstreamer :q :dsp)
  :build-operation monolithic-compile-bundle-op
  :build-pathname "prelude")
