;;; ~/comp/core/lisp/app/homer/homer.asd --- Homer Sytem Definitions
(defsystem :homer
  :depends-on (:core :krypt :skel :io :pod :box :net :obj :dat :mpk)
  :components ((:file "pkg")
               (:file "var")
               (:file "log")
               (:file "util")
               (:file "task")
               (:file "cfg")
               (:file "srv")
               (:file "cli")
               (:file "homer")))
