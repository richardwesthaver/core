;;; pkg.lisp --- Apache Jpeg FFI

;; 

;;; Code:
(defpackage :jpeg
  (:use :cl :std :sb-alien)
  (:export :load-turbojpeg :load-jpeg
           :save-jpeg-image
           :with-jpeg-transformer
           :with-jpeg-decompressor
           :with-jpeg-compressor
           :jpeg-transformer
           :jpeg-decompressor
           :jpeg-compressor
           :jpeg
           :jpeg-get
           :jpeg-set
           :jpeg-error
           :jpeg-warning
           :transform-jpeg-image
           :load-jpeg-image))

(in-package :jpeg)

(define-alien-loader turbojpeg "/usr/lib/")
(define-alien-loader jpeg "/usr/lib/")
