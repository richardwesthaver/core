;;; parquet.lisp --- Apache Parquet

;; Common Lisp implementation of Apache Parquet

;;; Commentary:

#|
https://github.com/apache/parquet-format
https://github.com/apache/parquet-format/blob/master/src/main/thrift/parquet.thrift
https://github.com/apache/parquet-testing
https://github.com/apache/parquet-java
https://github.com/apache/arrow-rs
|#

;; In this package we're being as lazy as possible. To generate our own
;; encoder/decoder methods we depend on the file parquet.thrift in the
;; parquet-format repo above. The core skelfile includes a script to download
;; it and convert it to parquet.json (requires the thirft cli tool). We then
;; decode it with DAT/JSON and generate lisp classes, and types.

;; 
;;; Code:
(in-package :dat/parquet)
(eval-when (:compile-toplevel)
  (load-parquet))

(defgeneric parquet-read (value &optional stream))
(defgeneric parquet-write (value &optional stream))

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
