(defpackage :xkb/tests 
    (:use :cl :rt :xkb))
(in-package :xkb/tests)
(defsuite :xkb)
(in-suite :xkb)

(deftest xkb-basic ()
  (xkb:load-xkb)
  (is (= xkb:xkb-keysym-max 536870911)))
