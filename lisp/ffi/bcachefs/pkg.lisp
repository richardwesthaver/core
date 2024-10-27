;;; bcachefs.lisp --- BCACHEFS ioctl bindings

;;; Commentary:

;;; Code:
(defpackage bcachefs
  (:use :cl :std :sb-alien)
  (:export))

(in-package :bcachefs)

(defmacro define-bcachefs-ioctl () "Define a wrapper for IOCTLs exposed by BCACHEFS.")
