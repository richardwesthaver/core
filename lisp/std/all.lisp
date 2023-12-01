;; TODO replace with defpkg
(uiop:define-package :std/all
  (:nicknames :std)
  (:use-reexport
   :std/base
   :std/ana
   :std/pan
   :std/log
   :std/fu
   :std/fmt
   ;; :std/tbl ;; too many name conflicts - may need obj
   :std/sxp
   :std/cli
   :std/gui
   :std/alien
   :std/thread))
