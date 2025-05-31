;;; ~/comp/core/lisp/app/mpk/mpk.asd --- Mpk Sytem Definitions
(defsystem :mpk
  :depends-on (:core/user)
  :build-operation program-op
  :build-pathname "mpk"
  :entry-point "mpk/cli::start-mpk"
  :components ((:file "pkg")
               (:file "mpd")
	       (:file "var")
               (:file "proto")
               (:file "util")
               (:file "db")
               (:file "cfg")
               (:file "net")
               (:file "mpk")
               (:file "cli")
               (:file "gui")))
