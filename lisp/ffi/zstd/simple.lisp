;;; simple.lisp --- Zstd Simple API

;; 

;;; Code:
(in-package :zstd)

(define-alien-routine "ZSTD_compress" size-t
  (dst (* t))
  (dst-capacity size-t)
  (src (* t))
  (src-size size-t)
  (compression int))

(define-alien-routine "ZSTD_decompress" size-t
  (dst (* t)) (dst-capacity size-t)
  (src (* t)) (compressed-size size-t))
