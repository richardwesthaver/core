;;; ~/comp/core/lisp/app/homer/homer.asd --- Homer Sytem Definitions
(defsystem :homer
  :depends-on (:prelude :gui :krypt :skel)
  :components ((:file "pkg")
               (:file "homer")
               (:file "cli")
               :file "gui"))
