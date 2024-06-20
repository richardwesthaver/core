(pushnew :user *features*)
(defsystem :user
  :depends-on (:std :cli :doc :nlp
               :obj :skel :syn :organ
               :packy :parse :pod :rdb
               :krypt :gui :aud)
  :components ((:file "user"))
  :build-operation monolithic-compile-bundle-op
  :build-pathname "user")
