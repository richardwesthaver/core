;;; sys.lisp --- SYS Core Compatibility

;; An extension to the STD/DEFSYS package which provides support for all
;; remaining core libraries.

;;; Commentary:

;; This file should be loaded right after the stage-1 libraries (see system
;; dependencies) and before the stage-2 dependencies (remaining modules in the
;; core).

;;; Code:
(in-package :std-user)

(defpkg :sys
  (:use :std-lisp :obj/id)
  (:use-reexport :std/defsys))

(in-package :sys)
(in-readtable :core)

