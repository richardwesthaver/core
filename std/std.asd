;;; std.asd --- standard library
(pushnew :std *features*)

(defsystem :std/named-readtables
  :version "0.1.0"
  :components ((:file "named-readtables"))
  :in-order-to ((test-op (test-op "std/tests"))))

(register-system-packages "std/named-readtables" '(:std))

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

(defsystem :std
  :version "0.1.0"
  :depends-on (:std/named-readtables)
  :serial t
  :components ((:file "defpkg")
               (:file "pkg")
               (:file "condition")
               (:file "sym")
               (:file "list")
               (:file "prim")
               (:file "type")
               (:module "num"
                :components
                ((:file "float")
                 (:file "parse")
                 (:file "leb128")
                 (:file "math")))
               (:file "stream")
               (:file "curry")
               (:file "array")
               (:file "hash-table")
               (:file "readtable")
               (:module "macs"
                :components
                ((:file "ana")
                 (:file "pan")
                 (:file "const")
                 (:file "var")
                 (:file "collecting")
                 (:file "loop")
                 (:file "control")
                 (:file "unit")
                 (:file "sugar")))
               (:file "pipe")
               (:file "sys")
               (:file "serde")
               (:file "alien")
               (:file "meta")
               (:file "bit")
	       (:file "spin")
	       (:file "seq")
               (:file "thread")
               (:file "task")
               (:file "async")
               (:file "par")
               (:file "fmt")
               (:file "path")
               (:file "os")
               (:file "file")
               (:file "string")
               (:file "rand"))
  :build-pathname "../.stash/std"
  :build-operation asdf:monolithic-compile-bundle-op
  :in-order-to ((test-op (test-op "std/tests"))))

(defsystem :std/tests
  :depends-on (:std :rt)
  :serial t
  :components ((:module "tests"
                :components
                ((:file "pkg")
                 (:file "num")
                 (:file "thread")
                 (:file "task"))))
  :perform (test-op (o c) (symbol-call :rt :do-tests :std)))
