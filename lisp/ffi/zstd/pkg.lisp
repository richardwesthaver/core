;;; ffi/zstd/pkg.lisp --- ZSTD FFI

;; Zstd compression support for Lisp

;;; Commentary:

;; The following programs have compile-time flags which can be used to enable
;; internal Zstd support:

;; SBCL
;; QEMU
 
#| from zstd.h:
  Introduction

  zstd, short for Zstandard, is a fast lossless compression algorithm, targeting
  real-time compression scenarios at zlib-level and better compression ratios.
  The zstd compression library provides in-memory compression and decompression
  functions.

  The library supports regular compression levels from 1 up to ZSTD_maxCLevel(),
  which is currently 22. Levels >= 20, labeled `--ultra`, should be used with
  caution, as they require more memory. The library also offers negative
  compression levels, which extend the range of speed vs. ratio preferences.
  The lower the level, the faster the speed (at the cost of compression).

  Compression can be done in:
    - a single step (described as Simple API)
    - a single step, reusing a context (described as Explicit context)
    - unbounded multiple steps (described as Streaming compression)

  The compression ratio achievable on small data can be highly improved using
  a dictionary. Dictionary compression can be performed in:
    - a single step (described as Simple dictionary API)
    - a single step, reusing a dictionary (described as Bulk-processing
      dictionary API)

  Advanced experimental functions can be accessed using
  `#define ZSTD_STATIC_LINKING_ONLY` before including zstd.h.

  Advanced experimental APIs should never be used with a dynamically-linked
  library. They are not "stable"; their definitions or signatures may change in
  the future. Only static linking is allowed.
|#

;;; Code:
(defpackage :zstd
  (:use :cl :std :sb-alien)
  (:nicknames :zstd))

(in-package :zstd)

(define-alien-loader "zstd" t "/usr/lib/")

;;; Utils
(define-alien-routine "ZSTD_versionNumber"
  unsigned)

(define-alien-routine "ZSTD_versionString" c-string)

(define-alien-routine "ZSTD_compressBound" size-t (src-size size-t))
(define-alien-routine "ZSTD_isError" unsigned (code size-t))
(define-alien-routine "ZSTD_getErrorName" c-string (code size-t))
(define-alien-routine "ZSTD_minCLevel" int)
(define-alien-routine "ZSTD_maxCLevel" int)
(define-alien-routine "ZSTD_defaultCLevel" int)

;;; Simple API
(define-alien-routine "ZSTD_compress" size-t
  (dst (* t)) (dst-capacity size-t)
  (src (* t)) (src-size size-t)
  (compression int))

(define-alien-routine "ZSTD_decompress" size-t
  (dst (* t)) (dst-capacity size-t)
  (src (* t)) (compressed-size size-t))

;;; Explicit Context API
(define-alien-type zstd-cctx (struct zstd-cctx-s))

(define-alien-routine "ZSTD_createCCtx" (* zstd-cctx))
(define-alien-routine "ZSTD_freeCCtx" void (cctx (* zstd-cctx)))
(define-alien-routine "ZSTD_compressCCtx" size-t
  (cctx (* zstd-cctx))
  (dst (* t)) (dst-capacity size-t)
  (src (* t)) (src-size size-t)
  (compression-level int))

(define-alien-type zstd-dctx (struct zstd-dctx-s))

(define-alien-routine "ZSTD_createDCtx" (* zstd-dctx))
(define-alien-routine "ZSTD_freeDCtx" void (dctx (* zstd-dctx)))
(define-alien-routine "ZSTD_decompressDCtx" size-t
  (dctx (* zstd-dctx))
  (dst (* t)) (dst-capacity size-t)
  (src (* t)) (src-size size-t))

;;; Streaming API
(define-alien-type zstd-inbuffer (struct zstd-inbuffer-s))


(define-alien-type zstd-outbuffer (struct zstd-outbuffer-s))

(define-alien-type zstd-cstream zstd-cctx)

(define-alien-routine "ZSTD_createCStream" (* zstd-cstream))
(define-alien-routine "ZSTD_freeCStream" void (zcs (* zstd-cstream)))

(define-alien-type zstd-enddirective int)
;; (enum nil
;;       (zstd-e-continue 0)
;;       (zstd-e-flush 1)
;;       (zstd-e-end 2))

(define-alien-routine "ZSTD_compressStream2" size-t
  (cctx (* zstd-cctx))
  (output (* zstd-outbuffer))
  (input (* zstd-inbuffer))
  (end-op zstd-enddirective))

(define-alien-routine "ZSTD_CStreamInSize" size-t)
(define-alien-routine "ZSTD_CStreamOutSize" size-t)
(define-alien-routine "ZSTD_initCStream" size-t (zcs (* zstd-cstream)) (compression-level int))

(define-alien-routine "ZSTD_compressStream" size-t (zcs (* zstd-cstream)) (output (* zstd-outbuffer)) (input (* zstd-inbuffer)))
(define-alien-routine "ZSTD_flushStream" size-t (zcs (* zstd-cstream)) (output (* zstd-outbuffer)))
(define-alien-routine "ZSTD_endStream" size-t (zcs (* zstd-cstream)) (output (* zstd-outbuffer)))

(define-alien-type zstd-dstream zstd-dctx)

(define-alien-routine "ZSTD_createDStream" (* zstd-dstream))
(define-alien-routine "ZSTD_freeDStream" void (zds (* zstd-dstream)))
(define-alien-routine "ZSTD_initDStream" size-t (zds (* zstd-dstream)))

(define-alien-routine "ZSTD_decompressStream" size-t
  (zds (* zstd-dstream))
  (output (* zstd-outbuffer))
  (input (* zstd-inbuffer)))

(define-alien-routine "ZSTD_DStreamInSize" size-t)
(define-alien-routine "ZSTD_DStreamOutSize" size-t)

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
