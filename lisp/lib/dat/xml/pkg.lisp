;;; dat/xml/pkg.lisp --- XML-like formats

;;; Code:
(in-package :dat/xml)

(defpackage :dat/fixml
  (:use :cl :std :dat/xml :dat/proto))

(defpackage :dat/svg
  (:use :cl :cl-ppcre :std :dat/xml :dat/proto)
  (:export :parse-svg-file :parse-svg-string))
