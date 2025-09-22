;;; print.lisp --- JS Printer

;; 

;;; Code:
(in-package :syn/gen/js)

(defvar *js-string-delimiter* #\'
  "Specifies which character should be used for delimiting strings.

This variable is used when you want to embed the resulting JavaScript
in an html attribute delimited by \" as opposed to ', or
vice-versa.")
