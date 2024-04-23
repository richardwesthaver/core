;;; std.asd --- standard library
(defsystem :std/named-readtables
  :version "0.1.0"
  :components ((:file "pkg") (:file "named-readtables"))
  :in-order-to ((test-op (test-op "std/tests"))))

(register-system-packages "std/named-readtables" '(:std))

(defsystem :std
  :version "0.1.0"
  :depends-on (:std/named-readtables :cl-ppcre :sb-concurrency)
  :serial t
  :components ((:file "pkg")
               (:file "defpkg")
               (:file "err")
               (:file "sym")
               (:file "list")
               (:file "type")
               (:module "num"
                :components
                ((:file "float")
                 (:file "parse")))
               (:file "stream")
               (:module "fu"
                :components
                ((:file "curry")))
               (:file "array")
               (:file "hash-table")
               (:file "alien")
               (:file "mop")
               (:file "thread")
               (:module "macs"
                :components
                ((:file "ana")
                 (:file "pan")
                 (:file "const")))
               (:file "bit")
               (:file "fmt")
               (:file "path")
               (:file "os")
               (:file "file")
               (:file "string")
               (:file "seq")
               (:file "sys")
               (:file "readtable"))
  :in-order-to ((test-op (test-op "std/tests"))))

(register-system-packages "std" '(:std))

(defsystem :std/tests
  :depends-on (:std :rt)
  :serial t
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :std)))
