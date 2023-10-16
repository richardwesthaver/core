;;; std/fs.lisp --- Filesystem utils

;; TODO

;;; Commentary:

;; I think PN has a 'portable pathname library' in PAICL or
;; something. oldie but goodie for reference.

;;; Code:
(defpackage :std/fs
  (:nicknames :fs)
  (:use :cl :str :cond :fu)
  (:export))

(in-package :fs)
