;;; io.lisp --- Parquet IO

;;

;;; Code:
(in-package :dat/parquet)

;;; Read/Write
(defun parquet-write-magic (stream)
  (write-string +parquet-magic-number+ stream))

(defun parquet-read-magic (stream)
  (assert (= #.(char-code (aref +parquet-magic-number+ 0)) (read-byte stream)))
  (assert (= #.(char-code (aref +parquet-magic-number+ 1)) (read-byte stream)))
  (assert (= #.(char-code (aref +parquet-magic-number+ 2)) (read-byte stream)))
  (assert (= #.(char-code (aref +parquet-magic-number+ 3)) (read-byte stream))))

(defun parquet-read-unsigned (stream)
  (read-uleb128 stream))

(defun parquet-read-signed (stream)
  (read-leb128 stream))

(defun parquet-read-boolean (stream)
  (ecase (read-byte stream)
    (0 nil)
    (1 t)))

(defun parquet-file-stream-p (stream)
  "Assert the start and end of a file STREAM are the parquet magic bytes."
  (parquet-read-magic stream)
  ;; set position to end - 4
  (file-position stream (- (the fixnum (file-length stream)) 4))
  (parquet-read-magic stream))

(defun parquet-read-schema-element (stream))
(defun parquet-read-schema (stream)
  "Read a parquet-schema which is repeated list of parquet-schema-element."
  
  )

(defun parquet-read-file-meta-data (stream)
  "Read a parquet-file-meta-data object from STREAM."
  ;; version
  (make-instance 'parquet-file-meta-data
    :version (parquet-read-signed stream)
    :schema (list (parquet-read-signed stream))))

(defun parquet-read-footer (stream)
  "Read the footer of parquet data in STREAM."
  (parquet-file-stream-p stream)
  ;; set file-position, read metadata length and magic
  (file-position stream (- (file-length stream) 8))
  (let ((len (parquet-read-unsigned stream)))
    (file-position stream (- (file-length stream) 8 len))
    (parquet-read-file-meta-data stream)))
