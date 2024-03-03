(in-package :gui/wm/x11)

(defpackage :x11/stumpwm
  (:use :cl :std)
  (:shadowing-import-from :std :group)
  (:use :stumpwm)
  (:export))
