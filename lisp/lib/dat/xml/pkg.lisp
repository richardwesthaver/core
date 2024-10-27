;;; dat/xml/pkg.lisp --- XML-like formats

;;; Code:
(in-package :dat/xml)

(defpackage :dat/fixml
  (:nicknames :fixml)
  (:use :cl :dat/xml :dat/proto))

(defpackage :dat/svg
  (:nicknames :svg)
  (:use :cl :cl-ppcre :dat/xml :dat/proto)
  (:import-from :std/string :*whitespaces*)
  (:export :parse-svg-file :parse-svg-string))
