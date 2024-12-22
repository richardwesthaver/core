;;; ~/comp/core/lisp/app/homer/homer.asd --- Homer Sytem Definitions
(defsystem :homer
  :depends-on (:prelude #+gui :gui #+cli :cli :krypt :skel :io :pod :box :net :obj :dat :mpk)
  :components ((:file "pkg")
               (:file "var")
               (:file "obj")
               (:file "log")
               (:file "cfg")
               (:file "util")
               #+gui
               (:file "gui")
               #+cli
               (:file "cli")))
