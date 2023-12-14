(defpackage :gui/skel
    (:use :cl :std :gui :skel)
  (:export :main))

(in-package :gui/skel)

(defun run ()
  (print "OK"))

(def-gui ()
  (run))
