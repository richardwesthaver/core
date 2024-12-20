;;; ~/comp/core/lisp/app/mpk/mpk.asd --- Mpk Sytem Definitions
(defsystem :mpk
  :depends-on (:prelude :dsp)
  :components ((:file "pkg")
               (:file "var")
               (:file "util")
               (:file "db")))
