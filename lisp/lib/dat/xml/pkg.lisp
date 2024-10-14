;;; dat/xml/pkg.lisp --- XML-like formats

;;; Code:
(in-package :dat/xml)

(defpackage :dat/fixml
  (:use :cl :dat/xml :dat/proto))

(defpackage :dat/svg
  (:use :cl :cl-ppcre :dat/xml :dat/proto)
  (:import-from :std/string :*whitespaces*)
  (:export :parse-svg-file :parse-svg-string))
