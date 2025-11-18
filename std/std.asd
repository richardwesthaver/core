;;; std.asd --- standard library

;; the build op on the STD system system concatenates all dependency systems -
;; make sure CL-PPCRE is loaded first but not included in the build output.
#-cl-ppcre
(handler-case
    (or
     #+quicklisp (ql:quickload :cl-ppcre)
     (asdf:load-system :cl-ppcre))
  (error () (error "unable to load system CL-PPCRE - make sure you have it installed in a path
that ASDF can find, or have quicklisp installed.")))

(require 'sb-cltl2)
(require 'sb-concurrency)
(require 'sb-sprof)
(require 'sb-introspect)
(require 'sb-rotate-byte)
;; (require 'sb-grovel)

(defsystem :std
  :version "0.1.0"
  :description "CC Standard Library"
  :depends-on (:cl-ppcre)
  :serial t
  :components 
  ((:file "named-readtables")
   (:file "defpkg")
   (:file "pkg")
   (:file "condition")
   (:file "sym")
   (:file "list")
   (:file "prim")
   (:file "kernel")
   (:file "type")
   (:file "string")
   (:module "num"
    :components
    ((:file "float")
     (:file "parse")
     (:file "leb128")
     (:file "math")))
   (:file "stream")
   (:file "curry")
   (:file "array")
   (:file "hash")
   (:file "readtable")
   (:module "macs"
    :components
    ((:file "ana")
     (:file "pan")
     (:file "collecting")
     (:file "loop")
     (:file "control")
     (:file "unit")
     (:file "sugar")
     (:file "match")
     (:file "memo")))
   (:file "pipe")
   (:file "sys")
   (:file "serde")
   (:file "alien")
   (:file "comp")
   (:file "meta")
   (:file "bit")
   (:file "seq")
   (:file "thread")
   (:file "async")
   (:file "defpun")
   (:file "task")
   (:file "print")
   (:file "path")
   (:file "os")
   (:file "file")
   (:file "rand")
   (:file "defsys"))
  :build-pathname "../.stash/std"
  :in-order-to ((test-op (test-op "std/tests"))))

(defsystem :std/tests
  :depends-on (:std :rt)
  :description "CC Standard Library Tests"
  :serial t
  :components 
  ((:module "tests"
    :components
    ((:file "pkg")
     (:file "num")
     (:file "seq")
     (:file "pipe")
     (:file "thread")
     (:file "async")
     (:file "task")
     (:file "macs"))))
  :perform (test-op (o c) (symbol-call :rt :do-tests :std)))
