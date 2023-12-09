#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

(defsystem :cli
    :depends-on (:cli/skel))

(defsystem :cli/organ
  :build-operation program-op
  :build-pathname "organ"
  :entry-point "cli/organ::main"
  :depends-on (:uiop :cl-ppcre :std/all :std/cli :organ :nlp)
  :components ((:file "organ"))
  :in-order-to ((test-op (test-op "app/tests")))
  :perform (test-op (o c) (symbol-call :std/rt :do-tests :app)))


(defsystem :cli/homer
  :build-operation program-op
  :build-pathname "homer"
  :entry-point "cli/homer::main"
  :depends-on (:uiop :cl-ppcre :std/all :std/cli :organ :skel :nlp)
  :components ((:file "homer"))
  :in-order-to ((test-op (test-op "app/tests")))
  :perform (test-op (o c) (symbol-call :std/rt :do-tests :app)))

(defsystem :cli/rdb
  :build-operation "program-op"
  :build-pathname "rdb"
  :entry-point "cli/rdb::main"
  :depends-on (:uiop :cl-ppcre :std/all :rdb :dot)
  :components ((:file "rdb"))
  :in-order-to ((test-op (test-op "app/tests")))
  :perform (test-op (o c) (symbol-call :std/rt :do-tests :app)))

(defsystem :cli/skel
  :build-operation program-op
  :build-pathname "skel"
  :entry-point "cli/skel::main"
  :components ((:file "skel"))
  :depends-on (:uiop :cl-ppcre :std/all :std/cli :skel)
  :in-order-to ((test-op (test-op "app/tests")))
  :perform (test-op (o c) (symbol-call :std/rt :do-tests :app)))

(defsystem :cli/packy
  :build-operation program-op
  :build-pathname "packy"
  :entry-point "cli/packy::main"
  :components ((:file "packy"))
  :depends-on (:uiop :cl-ppcre :std/all :std/cli :packy)
  :in-order-to ((test-op (test-op "app/tests")))
  :perform (test-op (o c) (symbol-call :std/rt :do-tests :app)))
