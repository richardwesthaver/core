(defpackage :bin/packy
  (:use :cl :std :sb-ext :cli :packy)
  (:export :main))

(in-package :bin/packy)

(defun run ())

(defmain ()
  (run)
  (sb-ext:exit :code 0))
