;;; ~/comp/core/lisp/app/homer/homer.asd --- Homer Sytem Definitions
(defsystem :homer
  :depends-on (:prelude :gui)
  :components ((:file "pkg")))
