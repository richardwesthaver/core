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
(zstd:load-zstd)

;;; Vars
(defparameter *compression-buffer-size* 4096)
(defparameter *decompression-buffer-size* 4096)
(defparameter *default-compression-level* (zstd:zstd-defaultclevel))
(defvar *compression-types* (list :zstd)
  "List of available compression backend types. May be used as the value of
*COMPRESSION-TYPE*.")
(defvar *compression-type* :zstd
  "Primary compression backend used by this Lisp system. Must be one of
*COMPRESSION-TYPES* and defaults to :ZSTD.")
(defvar *compression-level* *default-compression-level*)
(defvar *compressor* nil
  "The global COMPRESSOR object.")
(defvar *decompressor* nil
  "The global DECOMPRESSOR object.")
;;; Utils

;;; Conditions
(eval-always (deferror flate-error () () (:auto t)))

(deferror compression-error (flate-error) () (:auto t))
(deferror decompression-error (flate-error) () (:auto t))

;;; Proto
(defgeneric finish-compression (self))
(defgeneric finish-decompression (self))
;; TODO 2024-06-08: maybe move this to generic io/stream protocol - 'RESET'

(defgeneric reset-compressor (self))
(defgeneric reset-decompressor (self))
(defgeneric make-compressing-stream (key &optional stream))
(defgeneric make-decompressing-stream (key &optional stream))
(defgeneric compress-object (obj))
(defgeneric decompress-object (obj))
(defgeneric compression-level (obj))
(defgeneric (setf compression-level) (new obj))
(defgeneric compress-with (self obj &key &allow-other-keys))
(defgeneric decompress-with (self obj &key &allow-other-keys))
;; from SALZA2
(defgeneric compress-octet-vector (vector compressor &key start end))
(defgeneric decompress-octet-vector (vector decompressor &key start end))

;;; Compression
;; AKA 'DEFLATE'

;; with-compressor
;; reset-compressor

(defclass compressing-stream (wrapped-stream) ()
  (:default-initargs
   :stream (make-instance 
               'fundamental-binary-output-stream)))

(defmethod make-compressing-stream ((key t) 
                                    &optional (stream
                                               (make-instance 'fundamental-binary-input-stream)))
  (make-instance 'compressing-stream :stream stream))

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

(defmethod make-decompressing-stream ((key t) 
                                      &optional (stream
                                                 (make-instance 'fundamental-binary-input-stream)))
  (make-instance 'decompressing-stream :stream stream))

(defclass decompressor (wrapped-stream)
  (input)
  (:default-initargs
   :stream (make-instance 'decompressing-stream)))
