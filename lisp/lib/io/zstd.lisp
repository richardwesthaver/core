;;; zstd.lisp --- Zstd IO API

;; High-level Zstd API

;;; Code:
(in-package :io/zstd)

(eval-always (deferror zstd-error (io-error) () (:auto t)))

(deferror zstd-input-error (zstd-error) () (:auto t))
(deferror zstd-output-error (zstd-error) () (:auto t))

(deferror zstd-checksum-error (zstd-error) () (:auto t))
(deferror zstd-dictionary-error (zstd-error) () (:auto t))

(defclass zstd-compressor (compressor) ())

(defclass zstd-decompressor (decompressor) ())
