;;; pkg.lisp --- Web Library

;; 

;;; Code:
(defpackage :web/sys
  (:use :cl :std))

(defpackage :web/html
  (:use :cl :std :dat/html :dat/xml :dat/sxp :obj/ast)
  (:export :with-html-output
           :with-html
           :html-output-stream
           :*html-output*
           :*html-lang*
           :*html-charset*))

(defpackage :web/css
  (:use :cl :std :dat/css :dat/xml))
