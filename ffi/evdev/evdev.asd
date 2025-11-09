;;; evdev.asd --- Evdev Sytem Definitions

;; libevdev for lisp.

;;; Commentary:

;; https://gitlab.freedesktop.org/libevdev/libevdev

;;; Code:
(defpackage :evdev.sys
  (:use :cl :asdf :sb-grovel :sb-alien))

(in-package :evdev.sys)

(defsystem :evdev
  :depends-on (:std)
  :components ((:file "pkg")
               (grovel-constants-file "constants" :package :evdev)
               (:file "input")
               (:file "dev"))
  :in-order-to ((test-op (test-op "evdev/tests"))))

(defsystem :evdev/tests
  :depends-on (:rt :evdev :xkb)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call :rt :do-tests :btrfs)))
