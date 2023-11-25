;;; app.asd --- application library
#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

(defsystem :app
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on 
  (:uiop :cl-ppcre :std/all :skel :organ :rdb 
   :app/cli/skel :app/cli/organ :app/cli/homer
   :app/gui/skel
   :app/web/index :app/web/dash)
  :in-order-to ((test-op (test-op "app/tests")))
  :perform (test-op (o c) (symbol-call :std/rt :do-tests :app))
  :build-operation "program-op"
  ;; :build-pathname "skel"
  :entry-point "main")

(defsystem :app/cli/skel
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :build-operation "program-op"
  :depends-on (:uiop :cl-ppcre :std/all :skel)
  :in-order-to ((test-op (test-op "app/tests")))
  :perform (test-op (o c) (symbol-call :std/rt :do-tests :app))
  :build-operation "program-op"
  :build-pathname "skel"
  :entry-point "app/cli/skel::main")

(defsystem :app/cli/organ
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :build-operation "program-op"
  :depends-on (:uiop :cl-ppcre :std/all :organ :nlp)
  :in-order-to ((test-op (test-op "app/tests")))
  :perform (test-op (o c) (symbol-call :std/rt :do-tests :app))
  :build-operation "program-op"
  :build-pathname "organ"
  :entry-point "app/cli/organ::main")

(defsystem :app/cli/homer
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :build-operation "program-op"
  :depends-on (:uiop :cl-ppcre :std/all :organ :skel :nlp)
  :in-order-to ((test-op (test-op "app/tests")))
  :perform (test-op (o c) (symbol-call :std/rt :do-tests :app))
  :build-operation "program-op"
  :build-pathname "homer"
  :entry-point "app/cli/homer::main")

(defsystem :app/cli/rdb
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :build-operation "program-op"
  :depends-on (:uiop :cl-ppcre :std/all :rdb)
  :in-order-to ((test-op (test-op "app/tests")))
  :perform (test-op (o c) (symbol-call :std/rt :do-tests :app))
  :build-operation "program-op"
  :build-pathname "rdb"
  :entry-point "app/cli/rdb::main")

(defsystem :app/web/index
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system :lass)
  :depends-on (:uiop :cl-ppcre :std/all :rdb :hunchentoot :parenscript :lass :spinneret :organ)
  :components ((:lass-file "web/style"))
  :build-operation "program-op"
  :in-order-to ((test-op (test-op "app/tests")))
  :perform (test-op (o c) (symbol-call :std/rt :do-tests :app))
  :build-pathname "web-index"
  :entry-point "app/web/index::main")

(defsystem :app/web/dash
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:uiop :cl-ppcre :std/all :rdb :hunchentoot :parenscript :lass :spinneret :organ)
  :build-operation "program-op"
  :in-order-to ((test-op (test-op "app/tests")))
  :perform (test-op (o c) (symbol-call :std/rt :do-tests :app))
  :build-pathname "web-dash"
  :entry-point "app/web/dash::main")
