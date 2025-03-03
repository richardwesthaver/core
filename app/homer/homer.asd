;;; ~/comp/core/lisp/app/homer/homer.asd --- Homer Sytem Definitions
(defsystem :homer
  :depends-on (:core
               (:feature :gui :gui) 
               (:feature :cli :cli) 
               :krypt :skel :io :pod :box :net :obj :dat :mpk)
  :components ((:file "pkg")
               (:file "var")
               (:file "log")
               (:file "util")
               (:file "task")
               (:file "cfg")
               (:file "srv")
               (:file "gui" :if-feature :gui)
               (:file "cli" :if-feature :cli)
               (:file "homer")))
