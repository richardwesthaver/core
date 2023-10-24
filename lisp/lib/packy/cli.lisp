(defpackage :packy/cli
  (:use :cl :std :sb-ext)
  (:export :main))

(in-package :packy/cli)

(defun run ())

(defmain ()
  (run)
  (sb-ext:exit :code 0))
