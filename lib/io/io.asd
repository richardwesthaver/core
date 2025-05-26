(defsystem :io
  :description "Lisp IO Interface"
  :depends-on (:cl-ppcre 
               :std
               :ironclad
               :uring
               :xkb
               :evdev
               :btrfs
               :sb-bsd-sockets :zstd :flexi-streams)
  :version "0.1.0"
  :serial t
  :components ((:file "pkg")
               (:file "socket")
               (:file "stream")
               (:file "static")
               (:file "fast")
               (:file "xsubseq")
               (:file "smart-buffer")
               (:file "uring" :if-feature :linux)
               (:file "proto")
               (:file "chunky")
               (:file "flate")
               (:file "zstd")
               (:file "zlib")
               (:module "disk"
                :components 
                ((:file "pkg")
                 (:file "util")
                 (:file "proto")
                 (:file "btrfs")))
               (:file "kbd")
               (:file "io"))
  :in-order-to ((test-op (test-op "io/tests"))))

(defsystem :io/tests
  :depends-on (:rt :io :uring :sb-bsd-sockets)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :io)))
