;;; skel/packy/pkgfile.lisp --- Pkgfile spec

;; Readers and Writers for pkgfiles.

;;; Commentary:

;;; Code:
(in-package :skel/packy)

(defclass pkgfile (sk-component) ()
  (:documentation "Package build files."))
