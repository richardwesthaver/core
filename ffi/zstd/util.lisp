;;; util.lisp --- ZSTD Utils

;; 

;;; Code:
(in-package :zstd)
(defar "ZSTD_sizeof_CCtx" size-t (cctx (* zstd-cctx)))
(defar "ZSTD_sizeof_DCtx" size-t (dctx (* zstd-dctx)))
(defar "ZSTD_sizeof_CStream" size-t (zcs (* zstd-cstream)))
(defar "ZSTD_sizeof_DStream" size-t (zds (* zstd-dstream)))
(defar "ZSTD_sizeof_CDict" size-t (cdict (* zstd-cdict)))
(defar "ZSTD_sizeof_DDict" size-t (ddict (* zstd-ddict)))
