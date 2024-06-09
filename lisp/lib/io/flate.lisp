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
;; compression backend offered.

;; We intend to almost exclusively support Zstd compression and decompression
;; using our ZSTD FFI Lisp system, so we'll make a new library - FLATE - which
;; provides a shared zstd compression/decompression to Lisp objects and
;; streams.

;;; Code:
(in-package :io/flate)

;;; Vars
(defparameter *compression-buffer-size* 4096)
(defparameter *decompression-buffer-size* 4096)

;;; Utils

;;; Proto
(deferror flate-error () () (:auto t))

(deferror compression-error (flate-error) () (:auto t))
(deferror decompression-error (flate-error) () (:auto t))

(defgeneric finish-compression (self))
(defgeneric finish-decompression (self))
;; TODO 2024-06-08: maybe move this to generic io/stream protocol - 'RESET'
(defgeneric reset-compressor (self))
(defgeneric reset-decompressor (self))
(defgeneric make-compressing-stream (compressor stream))
(defgeneric make-decompressing-stream (decompressor stream))
(defgeneric compress-object (self))
(defgeneric decompress-object (self))

(defgeneric compress (input state output))
(defgeneric decompress (input state output))

;; decompress

;;; Compression

;; AKA 'DEFLATE'

;; compress-octet
;; compress-octet-vector

;; finish-compression (finish-output?)
;; with-compressor
;; reset-compressor

;; make-compressing-stream

(defclass compressor ()
  ((input
    :initarg :input
    :accessor compressor-input)
   (start
    :initarg :start
    :accessor compressor-start)
   (end
    :initarg :end
    :accessor compressor-end)))

(defclass compressing-stream (fundamental-binary-output-stream)
  ((compressor
    :initarg :compressor
    :accessor compressor)))

;;; Decompression

;; AKA 'INFLATE'

;; From chipz:
;; We provide several convenience functions for decompression:
;;
;; * decompress a buffer to a newly-consed buffer;
;; * decompress a stream to a newly-consed buffer;
;; * decompress a pathname to a newly-consed buffer;
;; * decompress a buffer to a user-specified buffer;
;; * decompress a buffer to a stream;
;; * decompress a stream to a stream.
;; * decompress a pathname to another pathname;
;; * decompress a pathname to a stream;
;;
;; We do not provide stream->buffer decompression, as we have no way of
;; knowing how much to read from the stream to fill the buffer, no way
;; of determining what to do with possible state left in the
;; INFLATE-STATE that we used, etc.  Application-specific logic will
;; have to handle those bits.

;; make-decompressing-stream
;; decompress-octet
;; decompress-octet-vector

(defclass decompressor () ())

(defclass decompressing-stream (fundamental-binary-input-stream)
  ((decompressor
    :initarg :compressor
    :accessor decompressor)))

;;; API

;; zstd-stream
;; zstd-file
