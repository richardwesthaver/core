(pushnew :user *features*)
(defsystem :user
  :depends-on (:std :cli :doc :nlp
               :obj :skel :syn :organ
               :packy :parse :pod :rdb 
               :krypt :gui)
  :components ((:file "user"))
  :build-pathname "user")
