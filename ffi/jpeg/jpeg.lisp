;;; jpeg.lisp --- libjpeg FFI

;; 

;;; Code:
(in-package :jpeg)

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
;; +dctsize2+
(define-alien-type jcoef (array jblock 64))

(define-alien-type jquant-tbl
    (struct jquant-tbl
      (values (array unsigned-short #.+dctsize2+))
      (sent boolean)))
(define-alien-type jhuff-tbl
    (struct jhuff-tbl
      (bits (array unsigned-char 17))
      (values (array unsigned-char 256))
      (sent boolean)))

(define-alien-enum (j-color-space)
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

(define-alien-enum (j-dct-method)
  :islow 0
  :ifast 1
  :float 2)

(define-alien-enum (j-dither-mode)
  :none 0
  :ordered 1
  :fs 2)

(define-alien-type jpeg-component-info
    (struct jpeg-component-info
      (component-id int)
      (component-index int)
      (horizontal-sampling-factor int)
      (vertical-sampling-factor int)
      (quantization-table-no int)
      (dc-entropy-table int)
      (ac-entropy-table int)
      (width-in-blocks unsigned-int)
      (height-in-blocks unsigned-int)
      (dct-horizontal-scaled-size int)
      (dct-vertical-scaled-size int)
      (downsampled-width unsigned-int)
      (downsampled-height unsigned-int)
      (component-needed-p boolean)
      (mcu-width int)
      (mcu-height int)
      (mcu-blocks int)
      (mcu-sample-width int)
      (last-column-width int)
      (last-row-height int)
      (quantization-table (* t))
      (dct-table (* t))))

(define-alien-type jpeg-scan-info
    (struct jpeg-scan-info
      (components-in-scan int)
      (component-index (array int #.+max-comps-in-scan+))
      (ss int)
      (se int)
      (ah int)
      (al int)))

(define-alien-type jpeg-marker-struct
    (struct jpeg-marker-struct
      (next (* t))
      (marker unsigned-char)
      (original-length unsigned-int)
      (data-length unsigned-int)
      (data (* t))))

(define-alien-type jpeg-compress-struct
    (struct jpeg-compress-struct
      (error-manager (* t))
      (memory-manager (* t))
      (progress-manager (* t))
      (client-data (* t))
      (decompressor-p boolean)
      (global-state int)
      (destination-manager (* t))
      (image-width unsigned-int)
      (image-height unsigned-int)
      (input-components int)
      (input-color-space j-color-space)
      (input-gamma double)
      (scale-number unsigned-int)
      (scale-denominator unsigned-int)
      (scaled-width unsigned-int)
      (scaled-height unsigned-int)
      (data-precision int)
      (component-count int)
      (color-space j-color-space)
      (component-info (* t))
      (quantization-tables (array (* t) #.+max-quant-tbls+))
      (q-scale-factor (array int #.+max-quant-tbls+))
      (dc-tables (array (* t) #.+num-huff-tbls+))
      (ac-tables (array (* t) #.+num-huff-tbls+))
      (arith-dc-l (array unsigned-char #.+num-arith-tbls+))
      (arith-dc-u (array unsigned-char #.+num-arith-tbls+))
      (arith-dc-k (array unsigned-char #.+num-arith-tbls+))
      (scan-conut int)
      (scan-info (* t))
      (raw-data-in boolean)
      (arith-code boolean)
      (optimize-coding boolean)
      (ccir601-sampling boolean)
      (fancy-downsampling boolean)
      (smoothing-factor int)
      (dct-method j-dct-method)
      (restrat-interval unsigned-int)
      (restart-in-rows int)
      (write-jfif-header boolean)
      (jfif-major-version unsigned-char)
      (jfif-minor-version unsigned-char)
      (density-unit unsigned-char)
      (x-density unsigned-short)
      (y-density unsigned-short)
      (write-adobe-marker boolean)
      (next-scanline unsigned-int)
      (progressive-mode boolean)
      (max-horizontal-sampling-factor int)
      (max-vertical-sampling-factor int)
      (min-dct-horizontal-scaled-size int)
      (min-dct-vertical-scaled-size int)
      (total-imcu-rows unsigned-int)
      (components-in-scan int)
      (current-component-info (array (* t) #.+max-comps-in-scan+))
      (mcus-per-row unsigned-int)
      (mcu-rows-in-scan unsigned-int)
      (blocks-in-mcu unsigned-int)
      (mcu-membership (array int #.+c-max-blocks-in-mcu+))
      (ss int)
      (se int)
      (ah int)
      (al int)
      (block-size int)
      (natural-order (* t))
      (lim-se int)
      (compression-master (* t))
      (c-main-controller (* t))
      (c-prep-controller (* t))
      (c-coef-controller (* t))
      (marker-writer (* t))
      (color-converter (* t))
      (downsampler (* t))
      (forward-dct (* t))
      (entropy-encoder (* t))
      (script-space (* t))
      (script-space-size int)))

(define-alien-type jpeg-decompress-struct
    (struct jpeg-decompress-struct
      (error-manager (* t))
      (memory-manager (* t))
      (progress-manager (* t))
      (client-data (* t))
      (decompressor-p boolean)
      (global-state int)
      (source-manager (* t))
      (image-width unsigned-int)
      (image-height unsigned-int)
      (components int)
      (color-space j-color-space)
      (output-color-space j-color-space)
      (scale-number unsigned-int)
      (scale-denominator unsigned-int)
      (output-gamma double)
      (buffered-image boolean)
      (raw-data-out boolean)
      (dct-method j-dct-method)
      (fancy-upsampling boolean)
      (block-smoothing boolean)
      (quantize-colors boolean)
      (dither-mode j-dither-mode)
      (two-pass-quantize boolean)
      (enable-1-pass-quantization boolean)
      (enable-external-quantization boolean)
      (enable-2-pass-quantization boolean)
      (output-width unsigned-int)
      (output-height unsigned-int)
      (output-color-components int)
      (output-components int)
      (recommended-outbuffer-height int)
      (actual-number-of-colors int)
      (colormap (* t))
      (output-scanline unsigned-int)
      (input-scan-number int)
      (input-imcu-row unsigned-int)
      (outptu-scan-number int)
      (output-imcu-row unsigned-int)
      (coef-bits (array int #.+dctsize2+))
      (quantization-tables (array (* t) #.+max-quant-tbls+))
      (dc-huffman-tables (array (* t) #.+num-huff-tbls+))
      (ac-huffman-tables (array (* t) #.+num-huff-tbls+))
      (data-precision int)
      (component-info (* t))
      (baseline-p boolean)
      (progressive-mode boolean)
      (arith-code boolean)
      (arith-dc-l (array unsigned-char #.+num-arith-tbls+))
      (arith-dc-u (array unsigned-char #.+num-arith-tbls+))
      (arith-dc-k (array unsigned-char #.+num-arith-tbls+))
      (restart-interval unsigned-int)
      (saw-jfif-marker boolean)
      (jfif-major-version unsigned-char)
      (jfif-minor-version unsigned-char)
      (density-unit unsigned-char)
      (x-density unsigned-short)
      (y-density unsigned-short)
      (saw-adobe-marker boolean)
      (adobe-transform unsigned-char)
      (ccir601-sampling boolean)
      (marker-list (* t))
      (max-horizontal-sampling-factor int)
      (max-vertical-sampling-factor int)
      (min-dct-horizontal-scaled-size int)
      (min-dct-vertical-scaled-size int)
      (total-imcu-rows unsigned-int)
      (sample-range-limit (* t))
      (components-in-scan int)
      (current-component-info (array (* t) #.+max-comps-in-scan+))
      (mcus-per-row unsigned-int)
      (mcu-rows-in-scan unsigned-int)
      (blocks-in-mcu int)
      (mcu-membership (array int #.+c-max-blocks-in-mcu+))
      (ss int)
      (se int)
      (ah int)
      (al int)
      (block-size int)
      (natural-order (* t))
      (lim-se int)
      (unread-marker int)
      (decompression-master (* t))
      (d-main-controller (* t))
      (d-coef-controller (* t))
      (d-post-controller (* t))
      (input-controller (* t))
      (marker-reader (* t))
      (entropy-decoder (* t))
      (inverse-dct (* t))
      (upsampler (* t))
      (color-deconverter (* t))
      (color-quantizer (* t))))

(define-alien-type jpeg-error-mgr
    (struct jpeg-error-mgr
      (error-exit (* t))
      (emit-message (* t))
      (output-message (* t))
      (format-message (* t))
      (reset-error-manager (* t))
      (message-code int)
      (parameters (array int 8))
      (trace-level int)
      (warning-count long)
      (message-table (* t))
      (last-message int)
      (addon-message-table (* t))
      (first-addon-message int)
      (last-addon-message int)))

(define-alien-type jpeg-progress-mgr
    (struct jpeg-progress-mgr
      (monitor (* t))
      (pass-counter long)
      (pass-limit long)
      (completed-passes int)
      (total-passes int)))

(define-alien-type jpeg-destination-mgr
    (struct jpeg-destination-mgr
      (next-output-byte (* t))
      (free-in-buffer size-t)
      (init-destination (* t))
      (empty-output-buffer (* t))
      (term-destination (* t))))

(define-alien-type jpeg-source-mgr
    (struct jpeg-source-mgr
      (next-input-byte (* t))
      (bytes-in-buffer size-t)
      (init-source (* t))
      (fill-input-buffer (* t))
      (skip-input-data (* t))
      (resync-to-restart (* t))
      (term-source (* t))))

(define-alien-type jpeg-memory-mgr
    (struct jpeg-memory-mgr
      (alloc-small (* t))
      (alloc-large (* t))
      (alloc-sarray (* t))
      (alloc-barray (* t))
      (request-virt-sarray (* t))
      (request-virt-barray (* t))
      (realize-virt-arrays (* t))
      (access-virt-sarray (* t))
      (access-virt-barray (* t))
      (free-pool (* t))
      (self-destruct (* t))
      (max-memory-to-use long)
      (max-alloc-chunk long)))

(define-alien-type j-common-ptr (* (struct jpeg-common-struct)))
(define-alien-type j-compress-ptr (* (struct jpeg-compress-struct)))
(define-alien-type j-decompress-ptr (* (struct jpeg-decompress-struct)))

(defar jpeg-std-error (* jpeg-error-mgr) (err (* jpeg-error-mgr)))

(defar jpeg-createcompress void (cinfo j-compress-ptr) (version int) (structsize size-t))
(defar jpeg-createdecompress void (cinfo j-decompress-ptr) (version int) (structsize size-t))

(defar jpeg-destroy-compress void (cinfo j-compress-ptr))
(defar jpeg-destroy-decompress void (cinfo j-decompress-ptr))

(defar jpeg-set-defaults void (cinfo j-compress-ptr))

(defar jpeg-start-compress void (cinfo j-compress-ptr) (write-all-tables boolean))
(defar jpeg-finish-compress void (cinfo j-compress-ptr))

(defar jpeg-calc-jpeg-dimensions void (cinfo j-compress-ptr))

(defar jpeg-start-decompress boolean (cinfo j-decompress-ptr))
(defar jpeg-finish-decompress boolean (cinfo j-decompress-ptr))

(defar jpeg-has-multiple-scans boolean (cinfo j-decompress-ptr))
(defar jpeg-start-output boolean (cinfo j-decompress-ptr) (scan-number int))
(defar jpeg-finish-output boolean (cinfo j-decompress-ptr))
(defar jpeg-input-complete boolean (cinfo j-decompress-ptr))
(defar jpeg-new-colormap void (cinfo j-decompress-ptr))
(defar jpeg-consume-input int (cinfo j-decompress-ptr))

(defar jpeg-abort-compress void (cinfo j-compress-ptr))
(defar jpeg-abort-decompress void (cinfo j-decompress-ptr))
