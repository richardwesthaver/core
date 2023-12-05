;;; skel.lisp --- skeleton library

;; A hacker's project compiler.

;;; Commentary:

;;; Code:
(uiop:define-package :skel/pkg
  (:nicknames :skel)
  (:use :cl :cl-ppcre :std :sb-mop)
  (:use-reexport :skel/core :skel/comp)
  (:shadowing-import-from :uiop :pathname-parent-directory-pathname :read-file-forms))

(in-package :skel/pkg)
(defvar *skel-version* "0.1.0")
