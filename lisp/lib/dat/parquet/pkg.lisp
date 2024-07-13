;;; pkg.lisp --- Parquet Packages

;; 

;;; Code:
(in-package :dat/parquet)

(defpackage :dat/parquet/gen
  (:use :cl :std :dat/proto :dat/json)
  (:export :load-parquet))
