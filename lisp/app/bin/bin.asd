#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

(defsystem :bin
  :depends-on (:bin/organ :bin/homer :bin/rdb :bin/skel :bin/packy)
  :in-order-to ((test-op (test-op "app/tests")))
  :perform (test-op (o c) (symbol-call :rt :do-tests :app)))

(defsystem :bin/organ
  :build-operation program-op
  :build-pathname "organ"
  :entry-point "bin/organ::main"
  :depends-on (:uiop :cl-ppcre :std :cli :organ :nlp)
  :components ((:file "organ"))
  :in-order-to ((test-op (test-op "app/tests")))
  :perform (test-op (o c) (symbol-call :rt :do-tests :app)))

(defsystem :bin/homer
  :build-operation program-op
  :build-pathname "homer"
  :entry-point "bin/homer::main"
  :depends-on (:uiop :cl-ppcre :std :cli
               :organ :skel :nlp :rdb :packy)
  :components ((:file "homer"))
  :in-order-to ((test-op (test-op "app/tests")))
  :perform (test-op (o c) (symbol-call :rt :do-tests :app)))

(defsystem :bin/rdb
  :build-operation "program-op"
  :build-pathname "rdb"
  :entry-point "bin/rdb::main"
  :depends-on (:uiop :cl-ppcre :std :rdb :cli)
  :components ((:file "rdb"))
  :in-order-to ((test-op (test-op "app/tests")))
  :perform (test-op (o c) (symbol-call :rt :do-tests :app)))

(defsystem :bin/skel
  :build-operation program-op
  :build-pathname "skel"
  :entry-point "bin/skel:main"
  :components ((:file "skel"))
  :depends-on (:uiop :cl-ppcre :std :cli :skel)
  :in-order-to ((test-op (test-op "app/tests")))
  :perform (test-op (o c) (symbol-call :rt :do-tests :app)))

(defsystem :bin/packy
  :build-operation program-op
  :build-pathname "packy"
  :entry-point "bin/packy::main"
  :depends-on (:uiop :cl-ppcre :std :cli :packy :rdb)
  :components ((:file "packy"))
  :in-order-to ((test-op (test-op "app/tests")))
  :perform (test-op (o c) (symbol-call :rt :do-tests :app)))
