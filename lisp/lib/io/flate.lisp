;;; io/flate.lisp --- Compressed IO Interface

;; Use compression (ZSTD) with Lisp objects and streams.

;;; Commentary:

;; compression ref: https://www.xach.com/lisp/salza2/ (compression only)

;; decompression ref: https://github.com/sharplispers/chipz (decompression only)

;; The libraries above are the current state-of-the-art for compression and
;; decompression in Common Lisp. They are portable packages which depend on
;; gray streams. They loosely cover deflate, zlib, gzip, and bzip2 data.

;; The compression backends are themselves hand-coded in Common Lisp, making
;; them excellent reference material. However, we don't have much use for the
;; compression backends offered.

;; We intend to almost exclusively support Zstd compression and decompression
;; using our ZSTD FFI Lisp system, so we'll make a new library - FLATE - which
;; provides a shared zstd compression/decompression to Lisp objects and
;; streams.

;;; Code:
(in-package :io/flate)

;;; Vars
(defparameter *compression-buffer-size* 4096)
(defparameter *decompression-buffer-size* 4096)
(defparameter *default-compression-level* (zstd:zstd-defaultclevel))
(defvar *compression-level* *default-compression-level*)

;;; Utils

;;; Errors
(eval-always (deferror flate-error () () (:auto t)))

(deferror compression-error (flate-error) () (:auto t))
(deferror decompression-error (flate-error) () (:auto t))

;;; Proto
(defgeneric finish-compression (self))
(defgeneric finish-decompression (self))
;; TODO 2024-06-08: maybe move this to generic io/stream protocol - 'RESET'

(defgeneric reset-compressor (self))
(defgeneric reset-decompressor (self))
(defgeneric make-compressed-stream (&optional stream))
(defgeneric make-decompressed-stream (&optional stream))
(defgeneric compress-object (obj))
(defgeneric decompress-object (obj))
(defgeneric compression-level (obj))
(defgeneric (setf compression-level) (new obj))
(defgeneric compress-with (self obj &key &allow-other-keys))
(defgeneric decompress-with (self obj &key &allow-other-keys))

;;; Compression
;; AKA 'DEFLATE'

;; compress-octet-vector

;; finish-compression (finish-output?)
;; with-compressor
;; reset-compressor

(defclass compressing-stream (wrapped-stream) ()
  (:default-initargs
   :stream (make-instance 
               'fundamental-binary-output-stream)))

(defclass compressor (wrapped-stream)
  (output)
  (:default-initargs
   :stream (make-instance 
               'compressing-stream)))

;;; Decompression

;; AKA 'INFLATE'
(defclass decompressing-stream (wrapped-stream) ()
  (:default-initargs
   :stream (make-instance 
               'fundamental-binary-input-stream)))

(defclass decompressor (wrapped-stream)
  (input)
  (:default-initargs
   :stream (make-instance 'decompressing-stream)))
