(defpackage :gui/wl
  (:use :cl :std :gui/core :wayflan)
  (:export))

(defpackage :gui/wl/kbd
  (:use :cl :std :xkb :gui/wl))

(defpackage :gui/wl/shell
  (:use :cl :std :xkb :gui/wl))
