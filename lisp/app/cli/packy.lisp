(defpackage :cli/packy
  (:use :cl :std :sb-ext)
  (:export :main))

(in-package :cli/packy)

(defun run ())

(defmain ()
  (run)
  (sb-ext:exit :code 0))
