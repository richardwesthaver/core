;;; dict.lisp --- Zstd Dictionary API

;; 

;;; Code:
(in-package :zstd)

(define-alien-enum (zstd-dict-content-type int)
                   :auto 0
                   :raw-content 1
                   :full-dict 2)

(define-alien-enum (zstd-dict-load-method int)
                   :by-copy 0
                   :by-ref 1)

(define-alien-enum (zstd-force-ignore-checksum int)
                   :validate-checksum 0
                   :ignore-checksum 1)

(define-alien-enum (zstd-ref-multiple-ddicts int)
                   :ref-single-ddict 0
                   :ref-multiple-ddicts 1)

(define-alien-enum (zstd-dict-attach-pref int)
                   :default-attach 0
                   :force-attach 1
                   :force-copy 2
                   :force-load 3)

(define-alien-enum (zstd-literal-compression-mode int)
                   :auto 0
                   :huffman 1
                   :uncompressed 2)

(define-alien-enum (zstd-param-switch int)
                   :auto 0
                   :enable 1
                   :disable 2)

(define-alien-enum (zstd-frame-type int)
                   :frame 0
                   :skippable-frame 1)

(define-alien-enum (zstd-sequence-format int)
                   :no-block-delimiters 0
                   :explicit-block-delimiters 1)

;;; Simple Dictionary API
(define-alien-routine "ZSTD_compress_usingDict" size-t
  (cctx (* zstd-cctx))
  (dst (* t))
  (dst-capacity size-t)
  (src (* t))
  (src-size size-t)
  (dict (* t))
  (dict-size size-t)
  (compression-level int))

(define-alien-routine "ZSTD_decompress_usingDict" size-t
  (dctx (* zstd-dctx))
  (dst (* t))
  (dst-capacity size-t)
  (src (* t))
  (src-size size-t)
  (dict (* t))
  (dict-size size-t))

;;; Bulk-processing Dictionary API
(define-alien-type zstd-cdict (struct zstd-cdict-s))

(define-alien-routine "ZSTD_createCDict" (* zstd-cdict)
  (dict-buffer (* t))
  (dict-size size-t)
  (compression-level int))

(define-alien-routine "ZSTD_freeCDict" size-t (cdict (* zstd-cdict)))

(define-alien-routine "ZSTD_compress_usingCDict" size-t
  (cctx (* zstd-cctx))
  (dst (* t))
  (dst-capacity size-t)
  (src (* t))
  (src-size size-t)
  (cdict (* zstd-cdict)))

(define-alien-type zstd-ddict (struct zstd-ddict-s))

(define-alien-routine "ZSTD_createDDict" (* zstd-ddict)
  (dict-buffer (* t))
  (dict-size size-t))

(define-alien-routine "ZSTD_freeDDict" size-t (ddict (* zstd-ddict)))

(define-alien-routine "ZSTD_compress_usingDDict" size-t
  (dctx (* zstd-dctx))
  (dst (* t))
  (dst-capacity size-t)
  (src (* t))
  (src-size size-t)
  (ddict (* zstd-ddict)))

;; dictionary utils
(define-alien-routine "ZSTD_getDictID_fromDict" unsigned
  (dict (* t))
  (dict-size size-t))

(define-alien-routine "ZSTD_getDictID_fromCDict" unsigned
  (cdict (* zstd-cdict)))

(define-alien-routine "ZSTD_getDictID_fromDDict" unsigned
  (cdict (* zstd-ddict)))

(define-alien-routine "ZSTD_getDictID_fromFrame" unsigned
  (src (* t))
  (src-size size-t))
