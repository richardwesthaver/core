;;; ~/comp/core/lisp/app/homer/homer.asd --- Homer Sytem Definitions
(defsystem :homer
  :depends-on (:prelude 
               (:feature :gui :gui) 
               (:feature :cli :cli) 
               :krypt :skel :io :pod :box :net :obj :dat :mpk)
  :components ((:file "pkg")
               (:file "var")
               (:file "obj")
               (:file "log")
               (:file "cfg")
               (:file "util")
               (:file "gui" :if-feature :gui)
               (:file "cli" :if-feature :cli)
               (:file "homer")))
