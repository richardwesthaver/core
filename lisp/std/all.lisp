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
   :std/tbl
   :std/sxp
   :std/cli
   :std/gui
   :std/alien
   :std/thread))
