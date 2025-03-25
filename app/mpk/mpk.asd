;;; ~/comp/core/lisp/app/mpk/mpk.asd --- Mpk Sytem Definitions
(defsystem :mpk
  :depends-on (:prelude :dsp)
  :build-operation program-op
  :build-pathname "mpk"
  :entry-point "mpk/cli::start-mpk"
  :components ((:file "pkg")
               (:file "proto")
               (:file "var")
               (:file "util")
               (:file "db")
               (:file "mpd")
               (:file "cfg")
               (:file "cli")))
