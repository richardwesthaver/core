;;; std.asd --- standard library

;; TODO: move into std system?
(defsystem :std/named-readtables
  :version "0.1.0"
  :components ((:file "named-readtables"))
  :in-order-to ((test-op (test-op "std/tests"))))

(register-system-packages "std/named-readtables" '(:std))

(defsystem :std
  :version "0.1.0"
  :depends-on (:std/named-readtables :cl-ppcre :sb-concurrency)
  :serial t
  :components ((:file "defpkg")
               (:file "pkg")
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
               (:file "readtable")
               (:module "macs"
                :components
                ((:file "ana")
                 (:file "pan")
                 (:file "const")
                 (:file "collecting")))
               (:file "bit")
               (:file "fmt")
               (:file "path")
               (:file "os")
               (:file "file")
               (:file "string")
               (:file "seq")
               (:file "sys"))
  :in-order-to ((test-op (test-op "std/tests"))))

(register-system-packages "std" '(:std))

(defsystem :std/tests
  :depends-on (:std :rt)
  :serial t
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :std)))
