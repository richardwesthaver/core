;;; css.lisp --- Cascading Style Sheets

;; https://www.w3.org/Style/CSS/

;;; Commentary:

;; for a list of all properties refer to: https://www.w3.org/Style/CSS/all-properties.en.html

;; for other web data: https://github.com/mdn/data/tree/main

;;; Code:
(in-package :dat/css)

;; SHEET     ::= (BLOCK*)
;; BLOCK     ::= (:BLOCK SELECTOR PROPERTY*)
;; SELECTOR  ::= (string*)
;; PROPERTY  ::= (:PROPERTY string string)
