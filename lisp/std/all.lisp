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
   :std/sxp
   :std/cli
   :std/alien
   :std/thread))
