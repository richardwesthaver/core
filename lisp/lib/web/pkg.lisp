;;; pkg.lisp --- Web Library

;; 

;;; Code:
(defpackage :web/sys
  (:use :cl :std))

(defpackage :web/html
  (:use :cl :std :dat/html :dat/xml))

(defpackage :web/css
  (:use :cl :std :dat/css :dat/xml))
