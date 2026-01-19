(defsystem :bin)

(defsystem :bin/homer
  :build-operation program-op
  :build-pathname "homer"
  :entry-point "skel/homer/cli::start-homer"
  :depends-on (:std :cli :organ :skel :nlp :rdb))

(defsystem :bin/mpk
  :build-operation program-op
  :build-pathname "mpk"
  :entry-point "mpk/cli::start-mpk"
  :depends-on (:std :cli :mpk))

(defsystem :bin/skel
  :build-operation program-op
  :build-pathname "skel"
  :entry-point "skel/cli::start-skel"
  :depends-on (:std :cli :skel :swank))

(defsystem :bin/core
  :build-operation program-op
  :build-pathname "core"
  :entry-point "bin/core::dispatch-core"
  :components ((:file "core"))
  :depends-on 
  (:core :skel :mpk))
