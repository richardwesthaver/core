(defsystem :io
  :description "Lisp IO Interface"
  :depends-on (:cl-ppcre 
               :std :obj 
               (:feature :linux :uring) 
               (:feature :linux :xkb) 
               (:feature :linux :evdev)
               :sb-bsd-sockets :zstd :flexi-streams)
  :version "0.1.0"
  :serial t
  :components ((:file "pkg")
               (:file "fast")
               (:file "ring" :if-feature :linux)
               (:file "socket")
               (:file "stream")
               (:file "proto")
               (:file "flate")
               (:file "port")
               (:file "zstd")
               (:file "kbd")
               (:file "xsubseq")
               (:file "smart-buffer"))
  :in-order-to ((test-op (test-op "io/tests"))))

(defsystem :io/tests
  :depends-on (:rt :io :uring :sb-bsd-sockets)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :io)))
