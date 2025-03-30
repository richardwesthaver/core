;;; tests.lisp --- JACK FFI Tests

;; 

;;; Code:
(defpackage :jack/tests
  (:use :cl :std :log :rt :jack))
(in-package :jack/tests)
(defsuite :jack)
(in-suite :jack)
(load-jack)

;; v2 /approaches/ 2.0
;; version string also indicates pipewire version
(deftest sanity ()  (istype 'string (jack-get-version-string)))
  
(deftest basic ()
  (jack-init-audio)
  (jack-init-midi))
