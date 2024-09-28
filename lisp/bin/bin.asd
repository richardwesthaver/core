(defsystem :bin
  :depends-on (:bin/organ :bin/homer :bin/rdb :bin/skel :bin/packy :bin/core))

(defsystem :bin/organ
  :build-operation program-op
  :build-pathname "organ"
  :entry-point "bin/organ::start-organ"
  :depends-on (:uiop :cl-ppcre :std :cli :organ :nlp)
  :components ((:file "organ")))

(defsystem :bin/homer
  :build-operation program-op
  :build-pathname "homer"
  :entry-point "bin/homer::start-homer"
  :depends-on (:uiop :cl-ppcre :std :cli
               :organ :skel :nlp :rdb :packy :krypt)
  :components ((:file "homer")))

(defsystem :bin/rdb
  :build-operation "program-op"
  :build-pathname "rdb"
  :entry-point "bin/rdb::start-rdb"
  :depends-on (:uiop :cl-ppcre :std :rdb :cli)
  :components ((:file "rdb")))

(defsystem :bin/skel
  :build-operation program-op
  :build-pathname "skel"
  :entry-point "bin/skel::start-skel"
  :components ((:file "skel"))
  :depends-on (:uiop :cl-ppcre :std :cli :skel))

(defsystem :bin/skc
  :build-operation program-op
  :build-pathname "skc"
  :entry-point "bin/skc::start-skc"
  :components ((:file "skc"))
  :depends-on (:std :cli :vc))

(defsystem :bin/packy
  :build-operation program-op
  :build-pathname "packy"
  :entry-point "bin/packy::start-packy"
  :depends-on (:uiop :cl-ppcre :std :cli :packy :rdb)
  :components ((:file "packy")))

(defsystem :bin/core
  :build-operation program-op
  :build-pathname "core"
  :entry-point "bin/core::dispatch-core"
  :components ((:file "core"))
  :depends-on (:std :cli :log :bin/skel :bin/organ :bin/homer :bin/rdb :bin/packy))
