;;; proto.lisp --- Parquet Data Protocol

;; 

;;; Code:
(in-package :dat/parquet)

(defgeneric parquet-read (value &optional stream))
(defgeneric parquet-write (value &optional stream))

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
