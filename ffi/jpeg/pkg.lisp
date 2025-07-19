;;; pkg.lisp --- Apache Jpeg FFI

;; 

;;; Code:
(defpackage :jpeg
  (:use :cl :std :sb-alien)
  (:export :load-turbojpeg))

(in-package :jpeg)

(define-alien-loader turbojpeg "/usr/lib/")

(define-alien-enum (init-type int)
  :compress 0
  :decompress 1
  :transform 2)

(define-alien-enum (chrominance-sampling int)
  :unknown -1
  :444 0
  :422 1
  :420 2
  :gray 3
  :440 4
  :411 5
  :441 6)

(defun mcu-width (sample)
  (ecase sample
    ((0 :444) 8)
    ((1 :422) 16)
    ((2 :420) 16)
    ((3 :gray) 8)
    ((4 :440) 8)
    ((5 :411) 32)
    ((6 :441) 8)))

(defun mcu-height (sample)
  (ecase sample
    ((0 :444) 8)
    ((1 :422) 8)
    ((2 :420) 16)
    ((3 :gray) 8)
    ((4 :440) 16)
    ((5 :411) 8)
    ((6 :441) 32)))

(define-alien-enum (pixel-format int)
  :rgb 0
  :bgr 1
  :rgbx 2
  :bgrx 3
  :xbgr 4
  :xrgb 5
  :gray 6
  :rgba 7
  :bgra 8
  :abgr 9
  :argb 10
  :cmyk 11
  :unknown -1)

(defun red-offset (format)
  (ecase format
    ((0 :rgb) 0)
    ((1 :bgr) 2)
    ((2 :rgbx) 0)
    ((3 :bgrx) 2)
    ((4 :xbgr) 3)
    ((5 :xrgb) 1)
    ((6 :gray) NIL)
    ((7 :rgba) 0)
    ((8 :bgra) 2)
    ((9 :abgr) 3)
    ((10 :argb) 1)
    ((11 :cmyk) NIL)))

(defun green-offset (format)
  (ecase format
    ((0 :rgb) 1)
    ((1 :bgr) 1)
    ((2 :rgbx) 1)
    ((3 :bgrx) 1)
    ((4 :xbgr) 2)
    ((5 :xrgb) 2)
    ((6 :gray) NIL)
    ((7 :rgba) 1)
    ((8 :bgra) 1)
    ((9 :abgr) 2)
    ((10 :argb) 2)
    ((11 :cmyk) NIL)))

(defun blue-offset (format)
  (ecase format
    ((0 :rgb) 2)
    ((1 :bgr) 0)
    ((2 :rgbx) 2)
    ((3 :bgrx) 0)
    ((4 :xbgr) 1)
    ((5 :xrgb) 3)
    ((6 :gray) NIL)
    ((7 :rgba) 2)
    ((8 :bgra) 0)
    ((9 :abgr) 1)
    ((10 :argb) 3)
    ((11 :cmyk) NIL)))

(defun alpha-offset (format)
  (ecase format
    ((0 :rgb) NIL)
    ((1 :bgr) NIL)
    ((2 :rgbx) NIL)
    ((3 :bgrx) NIL)
    ((4 :xbgr) NIL)
    ((5 :xrgb) NIL)
    ((6 :gray) NIL)
    ((7 :rgba) 3)
    ((8 :bgra) 3)
    ((9 :abgr) 0)
    ((10 :argb) 0)
    ((11 :cmyk) NIL)))

(defun pixel-size (format)
  (ecase format
    ((0 :rgb) 3)
    ((1 :bgr) 3)
    ((2 :rgbx) 4)
    ((3 :bgrx) 4)
    ((4 :xbgr) 4)
    ((5 :xrgb) 4)
    ((6 :gray) 1)
    ((7 :rgba) 4)
    ((8 :bgra) 4)
    ((9 :abgr) 4)
    ((10 :argb) 4)
    ((11 :cmyk) 4)))

(define-alien-enum (color-space int)
  :rgb 0
  :ycbcr 1
  :gray 2
  :cmyk 3
  :ycck 4)

(define-alien-enum (parameter int)
  :stop-on-warning 0
  :bottom-up 1
  :no-realloc 2
  :quality 3
  :subsampling 4
  :width 5
  :height 6
  :precision 7
  :color-space 8
  :fast-upsample 9
  :fast-dct 10
  :optimize 11
  :progressive 12
  :scan-limit 13
  :arithmetic 14
  :lossless 15
  :lossless-psv 16
  :lossless-pt 17
  :restart-blocks 18
  :restart-rows 19
  :x-density 20
  :y-density 21
  :density-units 22)

(define-alien-enum (error-type int)
  :warning 0
  :fatal 1)

(define-alien-enum (operation int)
  :none 0
  :horizontal-flip 1
  :vertical-flip 2
  :transpose 3
  :transverse 4
  :rotate-90 5
  :rotate-180 6
  :rotate-270 7)

(define-alien-enum (option unsigned-char)
  :perfect     #b000000001
  :trim        #b000000010
  :crop        #b000000100
  :gray        #b000001000
  :no-output   #b000010000
  :progressive #b000100000
  :copy-none   #b001000000
  :arithmetic  #b010000000
  :optimize    #b100000000)

(define-alien-type scaling-factor
    (struct scaling-factor
      (numerator int)
      (denominator int)))

(define-alien-type region
    (struct region
      (x int)
      (y int)
      (w int)
      (h int)))

(define-alien-type transform
    (struct transform
      (region region)
      (operation operation)
      (options option)
      (data (* t))
      (custom-filter (* t))))

(defar ("tj3Init" tj3init) (* t)
  (type init-type))

(tj3init (init-type :compress))
