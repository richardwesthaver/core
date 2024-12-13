;;; pkg.lisp --- MPK Packages

;; 

;;; Code:
(defpackage :mpk/int
  (:use :cl :std :log)
  (:export
   #:*mpk-directory*
   #:mpk-path))

(defpackage :mdb
  (:use :cl :std :log :rdb :dsp/aud :dsp/gst :mpk/int :schema :db))

(defpackage :mpk
  (:use :cl :std :log :mpk/int))

(defpackage :mpk/cli
  (:use :cl :std :log :mpk))
