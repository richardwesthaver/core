(defpackage :xkb/tests 
    (:use :cl :rt :std :xkb))
(in-package :xkb/tests)
(defsuite :xkb)
(in-suite :xkb)

(load-xkb)

(deftest xkb-basic ()
  (is (= xkb:xkb-keysym-max 536870911)))
