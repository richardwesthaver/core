;;; dict.lisp --- Zstd Dictionary API

;; 

;;; Code:
(in-package :zstd)

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
