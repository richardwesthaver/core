;;; std.asd --- standard library

(defsystem :std
  :version "0.1.0"
  :description "CC Standard Library"
  :serial t
  :components 
  ((:file "defpkg")
   (:file "pkg")
   (:file "sym")
   (:file "list")
   (:file "prim")
   (:file "kernel")
   (:file "condition")
   (:file "named-readtables")
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
   (:file "hash")
   (:module "ppcre"
    :components
    ((:file "var")
     (:file "util")
     (:file "errors")
     (:file "charset")
     (:file "charmap")
     (:file "chartest")
     (:file "lexer")
     (:file "parser")
     (:file "regex-class")
     (:file "regex-class-util")
     (:file "convert")
     (:file "optimize")
     (:file "closures")
     (:file "repetition-closures")
     (:file "scanner")
     (:file "api")))
   (:file "readtable")
   (:module "macs"
    :components
    ((:file "ana")
     (:file "pan")
     (:file "control")
     (:file "sugar")
     (:file "match")
     (:file "unit")
     (:file "memo")))
   (:file "array")
   (:file "pipe")
   (:file "core")
   (:file "io")
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
