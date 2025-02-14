;;; util.lisp --- ZSTD Utils

;; 

;;; Code:
(in-package :zstd)
(define-alien-routine "ZSTD_sizeof_CCtx" size-t (cctx (* zstd-cctx)))
(define-alien-routine "ZSTD_sizeof_DCtx" size-t (dctx (* zstd-dctx)))
(define-alien-routine "ZSTD_sizeof_CStream" size-t (zcs (* zstd-cstream)))
(define-alien-routine "ZSTD_sizeof_DStream" size-t (zds (* zstd-dstream)))
(define-alien-routine "ZSTD_sizeof_CDict" size-t (cdict (* zstd-cdict)))
(define-alien-routine "ZSTD_sizeof_DDict" size-t (ddict (* zstd-ddict)))
