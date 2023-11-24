(uiop:define-package :app/gui/skel
    (:use :cl :std :std/gui :skel :skel/core/vc :skel/core/virt :skel/comp/make)
  (:export :gui-main))

(in-package :app/gui/skel)

(defun run ()
  (print "OK"))

(def-gui ()
  (run))
