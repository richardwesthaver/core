(pushnew :user *features*)
(defsystem :user
  :depends-on (:std :cli :doc :nlp
               :obj :skel :syn :organ
               :packy :parse :pod :rdb
               :krypt :gui :dsp :net
               :krypt :rt :vc :dat
               :q :box :log :gui
               :web)
  :components ((:file "user"))
  :build-operation monolithic-compile-bundle-op
  :build-pathname "user")
