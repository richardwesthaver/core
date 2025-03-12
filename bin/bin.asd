(pushnew :cli *features*)

;; #+sb-core-compression
;; (defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
;;   (uiop:dump-image (asdf:output-file o c)
;;                    :executable t
;;                    :compression 2))

(defsystem :bin
  :depends-on (:bin/organ :bin/homer 
               :bin/rdb :bin/skel 
	       :bin/packy :bin/core
	       :bin/vc :bin/skc
	       :bin/pod :bin/gen
               #+x11 :bin/swm))

#+x11
(defsystem :bin/swm
  :depends-on (:std :log :gui :cli)
  :build-operation program-op
  :build-pathname "swm"
  :entry-point "bin/swm::start-swm"
  :components ((:file "swm")))

(defsystem :bin/organ
  :build-operation program-op
  :build-pathname "organ"
  :entry-point "bin/organ::start-organ"
  :depends-on (:std :cli :organ :nlp)
  :components ((:file "organ")))

(defsystem :bin/homer
  :build-operation program-op
  :build-pathname "homer"
  :entry-point "bin/homer::start-homer"
  :depends-on (:std :cli :organ :skel :nlp :rdb :homer)
  :components ((:file "homer")))

(defsystem :bin/pod
  :build-operation program-op
  :build-pathname "pod"
  :entry-point "bin/pod::start-pod"
  :depends-on (:std :cli :pod)
  :components ((:file "pod")))

(defsystem :bin/rdb
  :build-operation "program-op"
  :build-pathname "rdb"
  :entry-point "bin/rdb::start-rdb"
  :depends-on (:std :rdb :cli)
  :components ((:file "rdb")))

(defsystem :bin/gen
  :build-operation "program-op"
  :build-pathname "gen"
  :entry-point "bin/gen::start-gen"
  :depends-on (:std :syn :cli)
  :components ((:file "gen")))

(defsystem :bin/vc
  :build-operation program-op
  :build-pathname "vc"
  :entry-point "bin/vc::start-vc"
  :depends-on (:std :cli :vc :log :obj)
  :components ((:file "vc")))

(defsystem :bin/skel
  :build-operation program-op
  :build-pathname "skel"
  :entry-point "bin/skel::start-skel"
  :components ((:file "skel"))
  :depends-on (:std :cli :skel :packy :krypt :swank))

(defsystem :bin/skc
  :build-operation program-op
  :build-pathname "skc"
  :entry-point "bin/skc::start-skc"
  :components ((:file "skc"))
  :depends-on (:std :skel))

(defsystem :bin/packy
  :build-operation program-op
  :build-pathname "packy"
  :entry-point "bin/packy::start-packy"
  :depends-on (:std :cli :packy :rdb)
  :components ((:file "packy")))

(defsystem :bin/core
  :build-operation program-op
  :build-pathname "core"
  :entry-point "bin/core::dispatch-core"
  :components ((:file "core"))
  :depends-on (:core :bin/skel :bin/organ :bin/homer :bin/rdb :bin/packy :bin/vc :bin/gen
                     (:feature :x11 :bin/swm)
                     :bin/pod))
