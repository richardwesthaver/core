;;; lib/skel/comp/pkgbuild.lisp --- Archlinux PKGBUILD compiler

;; 

;;; Code:
(in-package :skel/comp/pkgbuild)

(defparameter *default-pkgbuild* "PKGBUILD")

(defclass sk-pkgbuild-system (skel sk-meta)
  ())

(defclass sk-pkgbuild-component (skel)
  (type value))

(defmethod sk-compile ((self sk-pkgbuild-system) stream &key &allow-other-keys))

(defmethod sk-write-file ((self sk-pkgbuild-system) &key path))

(defmethod sk-read-file ((self sk-pkgbuild-system) path))
