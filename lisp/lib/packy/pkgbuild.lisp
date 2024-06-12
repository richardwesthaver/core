;;; lib/skel/comp/pkgbuild.lisp --- Archlinux PKGBUILDs

;; Readers and Writers for PKBUILD files

;;; Commentary:

;; wiki: https://wiki.archlinux.org/title/PKGBUILD
;; man: https://man.archlinux.org/man/PKGBUILD.5
;; ref: https://wiki.archlinux.org/title/Creating_packages

;;; Code:
(in-package :packy/pkgbuild)

(defparameter *pkgbuild-filename* "PKGBUILD")

(defclass pkgbuild (package-id)
  (srcdir
   pkgdir))

(defmethod prepare-package ((self pkgbuild)))
(defmethod build-package ((self pkgbuild)))
(defmethod check-package ((self pkgbuild)))
(defmethod package-version ((self pkgbuild)))
(defmethod pack ((self pkgbuild)))
