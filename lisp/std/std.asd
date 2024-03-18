;;; std.asd --- standard library
(defsystem :std/named-readtables
  :version "0.1.0"
  :components ((:file "named-readtables"))
  :in-order-to ((test-op (test-op "std/tests"))))

(register-system-packages "std/named-readtables" '(:std))

(defsystem :std
  :version "0.1.0"
  :depends-on (:std/named-readtables :cl-ppcre :sb-concurrency)
  :serial t
  :components ((:file "pkg")
               (:file "defpkg")
               (:file "err")
               (:file "bits")
               (:module "num"
                :components ((:file "float")
                             (:file "parse")))
               (:file "str")
               (:file "fmt")
               (:file "sym")
               (:file "list")
               (:file "util")
               (:file "readtable")
               (:file "fu")
               (:file "ana")
               (:file "pan")
               (:file "thread")
               (:file "alien"))
  :in-order-to ((test-op (test-op "std/tests"))))

(register-system-packages "std" '(:std))

(defsystem :std/tests
  :depends-on (:std :rt)
  :serial t
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :std)))
