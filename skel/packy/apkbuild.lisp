;;; apkbuild.lisp --- APKBUILD file support

;; 

;;; Commentary:

;; ref: https://wiki.alpinelinux.org/wiki/APKBUILD_Reference

;;; Code:
(in-package :skel/packy/apkbuild)

(defparameter *apkbuild-filename* "APKBUILD")

(defclass apkbuild (package-id)
  (maintainer
   arch
   depends
   depends-dev
   depends-doc
   depends-openrc
   depends-libs
   depends-static
   checkdepends
   giturl
   install))

(defmethod prepare-package ((self apkbuild)))
(defmethod build-package ((self apkbuild)))
(defmethod check-package ((self apkbuild)))
(defmethod package-version ((self apkbuild)))
(defmethod pack ((self apkbuild)))
