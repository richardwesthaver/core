;;; turbo.lisp --- libjpeg-turbo FFI

;; 

;;; Code:
(in-package :jpeg)

(define-alien-enum (init-type)
  :compress 0
  :decompress 1
  :transform 2)

(define-alien-enum (chrominance-sampling)
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

(define-alien-enum (pixel-format)
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

(define-alien-enum (color-space)
  :rgb 0
  :ycbcr 1
  :gray 2
  :cmyk 3
  :ycck 4)

(define-alien-enum (parameter)
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

(define-alien-enum (error-type)
  :warning 0
  :fatal 1)

(define-alien-enum (operation)
  :none 0
  :horizontal-flip 1
  :vertical-flip 2
  :transpose 3
  :transverse 4
  :rotate-90 5
  :rotate-180 6
  :rotate-270 7)

(define-alien-enum (option :type unsigned-char)
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

(defar ("tj3Destroy" tj3destroy) void
  (handle (* t)))

(defar ("tj3GetErrorStr" tj3geterrorstr) c-string (handle (* t)))

(defar ("tj3GetErrorCode" tj3geterrorcode) int (handle (* t)))

(defar ("tj3Set" tj3set) int 
  (handle (* t))
  (param int)
  (value int))

(defar ("tj3Get" tj3get) int
  (handle (* t))
  (param int))

(defar ("tj3Alloc" tj3alloc) (* t)
  (bytes size-t))

(defar ("tj3Free" tj3free) void
  (buffer (* t)))

(defar ("tj3JPEGBufSize" tj3jpegbufsize) size-t (width int) (height int) (jpegsubsamp int))

(defar ("tj3YUVBufSize" tj3yuvbufsize) size-t (width int) (align int) (height int) (subsamp int))

(defar ("tj3YUVPlaneSize" tj3yuvplanesize) size-t 
  (component-id int) (width int) (stride int) (height int) (subsamp int))

(defar ("tj3YUVPlaneWidth" tj3yuvplanewidth) int (component-id int) (width int) (subsamp int))

(defar ("tj3YUVPlaneHeight" tj3yuvplaneheight) int (component-id int) (height int) (subsamp int))

(defar ("tj3SetICCProfile" tj3seticcprofile) int (handle (* t)) (iccbuf (* unsigned-char)) (icc-size size-t))

(defar ("tj3Compress8" tj3compress8) int 
  (handle (* t))
  (src-buf (* unsigned-char))
  (width int)
  (pitch int)
  (height int)
  (pixel-format int)
  (jpeg-buf (* (* unsigned-char)))
  (jpeg-size (* size-t)))

(defar ("tj3Compress12" tj3compress12) int 
  (handle (* t))
  (src-buf (* unsigned-char))
  (width int)
  (pitch int)
  (height int)
  (pixel-format int)
  (jpeg-buf (* (* unsigned-char)))
  (jpeg-size (* size-t)))

(defar ("tj3Compress16" tj3compress16) int 
  (handle (* t))
  (src-buf (* unsigned-char))
  (width int)
  (pitch int)
  (height int)
  (pixel-format int)
  (jpeg-buf (* (* unsigned-char)))
  (jpeg-size (* size-t)))

(defar ("tj3CompressFromYUVPlanes8" tj3compressfromyuvplanes8) int
  (handle (* t))
  (src-planes (* (* unsigned-char)))
  (width int)
  (strides (* int))
  (height int)
  (jpeg-buf (* (* unsigned-char)))
  (jpeg-size (* size-t)))

(defar ("tj3CompressFromYUV8" tj3compressfromyuv8) int
  (handle (* t))
  (src-buf (* unsigned-char))
  (width int)
  (align int)
  (height int)
  (jpeg-buf (* (* unsigned-char)))
  (jpeg-size (* size-t)))

(defar ("tj3EncodeYUVPlanes8" tj3encodeyuvplanes8) int
  (handle (* t))
  (src-buf (* unsigned-char))
  (width int)
  (pitch int)
  (height int)
  (pixel-format int)
  (dst-planes (* (* unsigned-char)))
  (strides (* int)))

(defar ("tj3EncodeYUV8" tj3encodeyuv8) int
  (handle (* t))
  (src-buf (* unsigned-char))
  (width int)
  (pitch int)
  (height int)
  (pixel-format int)
  (dst-buf (* unsigned-char))
  (align int))

(defar ("tj3DecompressHeader" tj3decompressheader) int
  (handle (* t))
  (jpeg-buf (* unsigned-char))
  (jpeg-size size-t))

(defar ("tj3GetICCProfile" tj3geticcprofile) int
  (handle (* t))
  (icc-buf (* (* unsigned-char)))
  (icc-size (* size-t)))

(defar ("tj3GetScalingFactors" tj3getscalingfactors) (* scaling-factor) (num-scaling-factors (* int)))

;; (defar ("tj3SetScalingFactor" tj3setscalingfactor) int (handle (* t)) (scaling-factor scaling-factor))
;; tj3SetCroppingRegion

(defar ("tj3Decompress8" tj3decompress8) int
  (handle (* t))
  (jpeg-buf (* unsigned-char))
  (jpeg-size size-t)
  (dst-buf (* unsigned-char))
  (pitch int)
  (pixel-format int))

(defar ("tj3Decompress12" tj3decompress12) int
  (handle (* t))
  (jpeg-buf (* unsigned-char))
  (jpeg-size size-t)
  (dst-buf (* unsigned-char))
  (pitch int)
  (pixel-format int))

(defar ("tj3Decompress16" tj3decompress16) int
  (handle (* t))
  (jpeg-buf (* unsigned-char))
  (jpeg-size size-t)
  (dst-buf (* unsigned-char))
  (pitch int)
  (pixel-format int))

(defar ("tj3DecompressToYUVPlanes8" tj3decompresstoyuvplanes8) int
  (handle (* t))
  (jpeg-buf (* unsigned-char))
  (jpeg-size size-t)
  (dst-planes (* (* unsigned-char)))
  (strides (* int)))

(defar ("tj3DecompressToYUV8" tj3decompresstoyuv8) int
  (handle (* t))
  (jpeg-buf (* unsigned-char))
  (jpeg-size size-t)
  (dst-buf (* unsigned-char))
  (align int))

(defar ("tj3DecodeYUVPlanes8" tj3decodeyuvplanes8) int
  (handle (* t))
  (src-planes (* (* unsigned-char)))
  (stides (* int))
  (dst-buf (* unsigned-char))
  (width int)
  (pitch int)
  (height int)
  (pixel-format int))

(defar ("tj3DecodeYUV8" tj3decodeyuv8) int
  (handle (* t))
  (src-buf (* unsigned-char))
  (align int)
  (dst-buf (* unsigned-char))
  (width int)
  (pitch int)
  (height int)
  (pixel-format int))

(defar ("tj3TransformBufSize" tj3transformbufsize) size-t
  (handle (* t))
  (transform (* transform)))

(defar ("tj3Transform" tj3transform) int
  (handle (* t))
  (jpeg-buf (* unsigned-char))
  (jpeg-size size-t)
  (n int)
  (dst-bufs (* (* unsigned-char)))
  (dst-sizes (* size-t))
  (transforms (* transform)))

(defar ("tj3LoadImage8" tj3loadimage8) (* unsigned-char)
  (handle (* t))
  (filename c-string)
  (width (* int))
  (align (* int))
  (height (* int))
  (pixel-format (* int)))

(defar ("tj3LoadImage12" tj3loadimage12) (* short)
  (handle (* t))
  (filename c-string)
  (width (* int))
  (align (* int))
  (height (* int))
  (pixel-format (* int)))

(defar ("tj3LoadImage16" tj3loadimage16) (* unsigned-short)
  (handle (* t))
  (filename c-string)
  (width (* int))
  (align (* int))
  (height (* int))
  (pixel-format (* int)))

(defar ("tj3SaveImage8" tj3saveimage8) int
  (handle (* t))
  (filename c-string)
  (buffer (* unsigned-char))
  (width int)
  (pitch int)
  (height int)
  (pixel-format int))

(defar ("tj3SaveImage12" tj3saveimage12) int
  (handle (* t))
  (filename c-string)
  (buffer (* unsigned-char))
  (width int)
  (pitch int)
  (height int)
  (pixel-format int))

(defar ("tj3SaveImage16" tj3saveimage16) int
  (handle (* t))
  (filename c-string)
  (buffer (* unsigned-char))
  (width int)
  (pitch int)
  (height int)
  (pixel-format int))
