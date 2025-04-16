;;; pkg.lisp --- Apache Jpeg FFI

;; 

;;; Code:
(defpackage :jpeg
  (:use :cl :std :sb-alien)
  (:export :load-jpeg))

(in-package :jpeg)

(define-alien-loader jpeg "/usr/lib/")

(define-alien-type jsampimage (* t))
(define-alien-type jsamparray (* jsampimage))
(define-alien-type jsamprow (* jsamparray))
(define-alien-type jsample (* jsamprow))

(define-alien-type j12sampimage (* t))
(define-alien-type j12samparray (* j12sampimage))
(define-alien-type j12samprow (* j12samparray))
(define-alien-type j12sample (* j12samprow))

(define-alien-type j16sampimage (* t))
(define-alien-type j16samparray (* j16sampimage))
(define-alien-type j16samprow (* j16samparray))
(define-alien-type j16sample (* j16samprow))

(define-alien-type jblockimage (* t))
(define-alien-type jblockarray (* jblockimage))
(define-alien-type jblockrow (* jblockarray))
(define-alien-type jblock (* jblockrow))
(define-alien-type jcoef (array jblock #.+dctsize2+))

;; jquant, jhuff
;; jpeg-component-info
;; jpeg-scan-info
;; jpeg-marker-struct

(define-alien-enum (j-color-space int)
  :unknown 0
  :grayscale 1
  :rgb 2
  :ycbcr 3
  :cmyk 4
  :ycck 5
  :ext-rgb 6
  :ext-rgbx 7
  :ext-bgr 8
  :ext-bgrx 9
  :ext-xbgr 10
  :ext-xrgb 11
  :ext-rgba 12
  :ext-bgra 13
  :ext-abgr 14
  :ext-argb 15
  :rgb565 16)

(define-alien-enum (j-dct-method int)
  :islow 0
  :ifast 1
  :float 2)

(define-alien-enum (j-dither-mode int)
  :none 0
  :ordered 1
  :fs 2)

;; jpeg-common-fields
;; ...

;; jpeg-compress-struct

;; jpeg-decompress-struct

;; jpeg-error-mgr
(define-alien-type jpeg-error-mgr (* t))
;; jpeg-progress-mgr, jpeg-destination-mgr, jpeg-source-mgr

;; jvirt-sarry-control, jvirt-barray-control

;; jpeg-memory-mgr

(define-alien-type j-common-ptr (* (struct jpeg-common-struct)))
(define-alien-type j-compress-ptr (* (struct jpeg-compress-struct)))
(define-alien-type j-decompress-ptr (* (struct jpeg-decompress-struct)))

(define-alien-routine jpeg-std-error (* jpeg-error-mgr) (err (* jpeg-error-mgr)))

(define-alien-routine jpeg-createcompress void (cinfo j-compress-ptr) (version int) (structsize size-t))
(define-alien-routine jpeg-createdecompress void (cinfo j-decompress-ptr) (version int) (structsize size-t))

(define-alien-routine jpeg-destroy-compress void (cinfo j-compress-ptr))
(define-alien-routine jpeg-destroy-decompress void (cinfo j-decompress-ptr))

(define-alien-routine jpeg-set-defaults void (cinfo j-compress-ptr))

(define-alien-routine jpeg-start-compress void (cinfo j-compress-ptr) (write-all-tables boolean))
(define-alien-routine jpeg-finish-compress void (cinfo j-compress-ptr))

(define-alien-routine jpeg-calc-jpeg-dimensions void (cinfo j-compress-ptr))

(define-alien-routine jpeg-start-decompress boolean (cinfo j-decompress-ptr))
(define-alien-routine jpeg-finish-decompress boolean (cinfo j-decompress-ptr))

(define-alien-routine jpeg-has-multiple-scans boolean (cinfo j-decompress-ptr))
(define-alien-routine jpeg-start-output boolean (cinfo j-decompress-ptr) (scan-number int))
(define-alien-routine jpeg-finish-output boolean (cinfo j-decompress-ptr))
(define-alien-routine jpeg-input-complete boolean (cinfo j-decompress-ptr))
(define-alien-routine jpeg-new-colormap void (cinfo j-decompress-ptr))
(define-alien-routine jpeg-consume-input int (cinfo j-decompress-ptr))

(define-alien-routine jpeg-abort-compress void (cinfo j-compress-ptr))
(define-alien-routine jpeg-abort-decompress void (cinfo j-decompress-ptr))
