(defsystem :bin)

(defsystem :bin/homer
  :build-operation program-op
  :build-pathname "homer"
  :entry-point "bin/homer::start-homer"
  :depends-on (:std :cli :organ :skel :nlp :rdb)
  :components ((:file "homer")))

(defsystem :bin/mpk
  :build-operation program-op
  :build-pathname "mpk"
  :entry-point "bin/mpk::start-mpk"
  :depends-on (:std :cli :mpk)
  :components ((:file "mpk")))

(defsystem :bin/skel
  :build-operation program-op
  :build-pathname "skel"
  :entry-point "bin/skel::start-skel"
  :components ((:file "skel"))
  :depends-on (:std :cli :skel :swank))

(defsystem :bin/core
  :build-operation program-op
  :build-pathname "core"
  :entry-point "bin/core::dispatch-core"
  :components ((:file "core"))
  :depends-on 
  (:core :bin/skel :bin/homer :bin/mpk))
         
         
