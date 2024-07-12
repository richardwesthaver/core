;;; pkg.lisp --- Apache Arrow FFI

;; 

;;; Code:
(defpackage :arrow
  (:use :cl :std :sb-alien)
  (:export))

(in-package :arrow)

(define-alien-loader "arrow" t "/usr/lib/")

(define-alien-type arrow-release-function (function void (* (struct nil))))

(define-alien-type arrow-schema
  (struct arrow-schema
          (format c-string)
          (name c-string)
          (metadata c-string)
          (flags long)
          (n-children long)
          (children (array (* (struct arrow-schema))))
          (dictionary (* (struct arrow-schema)))
          (release (* arrow-release-function))
          (private-data (* t))))


(define-alien-type arrow-array
  (struct arrow-array
          (length long)
          (null-count long)
          (offset long)
          (n-buffers long)
          (n-children long)
          (buffers (array (* t)))
          (children (array (* (struct arrow-array))))
          (dictionary (* (struct arrow-array)))
          (release (* arrow-release-function))
          (private-data (* t))))
