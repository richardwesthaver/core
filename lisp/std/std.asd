;;; std.asd --- standard library
(pushnew :std *features*)

(defsystem :std/named-readtables
  :version "0.1.0"
  :components ((:file "named-readtables"))
  :in-order-to ((test-op (test-op "std/tests"))))

(register-system-packages "std/named-readtables" '(:std))

;; the build op on the STD system system concatenates all dependency systems -
;; make sure CL-PPCRE is loaded first but not included in the build output.
(asdf:load-system :asdf)

(require 'sb-cltl2)
(require 'sb-concurrency)

(defsystem :std
  :version "0.1.0"
  :depends-on (:std/named-readtables)
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
               (:file "readtable")
               (:module "macs"
                :components
                ((:file "ana")
                 (:file "pan")
                 (:file "const")
                 (:file "collecting")
                 (:file "control")))
               (:file "bit")
               (:file "thread")
               (:file "fmt")
               (:file "path")
               (:file "os")
               (:file "file")
               (:file "string")
               (:file "seq")
               (:file "sys"))
  :build-pathname "std"
  :build-operation monolithic-concatenate-source-op
  :in-order-to ((test-op (test-op "std/tests"))))

(register-system-packages "std" '(:std))

(defsystem :std/tests
  :depends-on (:std :rt)
  :serial t
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :std)))
