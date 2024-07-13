;;; parquet.lisp --- Apache Parquet

;; Common Lisp implementation of Apache Parquet

;;; Commentary:

#|
https://github.com/apache/parquet-format
https://github.com/apache/parquet-format/blob/master/src/main/thrift/parquet.thrift
https://github.com/apache/parquet-testing
https://github.com/apache/parquet-java
https://github.com/apache/arrow-rs

https://thrift.apache.org/docs/types
|#

#|
    4-byte magic number "PAR1"
    <Column 1 Chunk 1>
    <Column 2 Chunk 1>
    ...
    <Column N Chunk 1>
    <Column 1 Chunk 2>
    <Column 2 Chunk 2>
    ...
    <Column N Chunk 2>
    ...
    <Column 1 Chunk M>
    <Column 2 Chunk M>
    ...
    <Column N Chunk M>
    File Metadata
    4-byte length in bytes of file metadata (little endian)
    4-byte magic number "PAR1"
|#

;; In this package we're being as lazy as possible. To generate our own
;; encoder/decoder methods we depend on the file parquet.thrift in the
;; parquet-format repo above. The core skelfile includes a script to download
;; it and convert it to parquet.json (requires the thirft cli tool). We then
;; decode it with DAT/JSON and generate lisp classes, and types.

;; 
;;; Code:
(in-package :dat/parquet)
(eval-always
  (dat/parquet/gen::load-parquet))

(defgeneric parquet-read (value &optional stream))
(defgeneric parquet-write (value &optional stream))

;;  HACK 2024-07-12: 
(define-bitfield parquet-compression-codec
  (uncompressed boolean)
  (snappy boolean)
  (gzip boolean)
  (lzo boolean)
  (brotli boolean)
  (lz4 boolean)
  (zstd boolean)
  (lz4-raw boolean))

;;; Read/Write
(define-constant +parquet-magic-number+ "PAR1" :test 'equal)

(defconstant +default-parquet-page-size+ (* 8 1024)) ;; 8kb
(defconstant +default-parquet-row-group-size (expt 1024 3)) ;; 1gb

(defvar *parquet-creator* "parquet-cl version 0.1.0")

(defun parquet-write-magic (stream)
  (write-string +parquet-magic-number+ stream))

(defun parquet-read-magic (stream)
  (assert (char= #.(char +parquet-magic-number+ 0) (read-char stream)))
  (assert (char= #.(char +parquet-magic-number+ 1) (read-char stream)))
  (assert (char= #.(char +parquet-magic-number+ 2) (read-char stream)))
  (assert (char= #.(char +parquet-magic-number+ 3) (read-char stream)))
  t)

(defmethod parquet-write ((value (eql t)) &optional stream)
  "Encode a parquet boolean true value."
  (declare (ignore value))
  (write-byte 1 stream))

(defmethod parquet-write ((value (eql nil)) &optional stream)
  "Encode a parquet boolean false value."
  (declare (ignore value))
  (write-byte 0 stream))

(defmethod parquet-write ((value string) &optional stream))

;;; Encode/Decode
(defun parquet-encode (value &optional stream)
  "Encode a Lisp value and write it to a parquet stream."
  (parquet-write value stream))

(defun parquet-decode (string &key (start 0) end)
  "Convert a PARQUET string into a Lisp object."
  (with-input-from-string (stream string :start start :end end)
    (values (parquet-read stream)
            (file-position stream))))
