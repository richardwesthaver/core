;;; io.lisp --- Parquet IO

;; 

;;; Code:
(in-package :dat/parquet)

;;; Read/Write
(defun parquet-write-magic (stream)
  (write-string +parquet-magic-number+ stream))

(defun parquet-read-magic (stream)
  (assert (char= #.(char +parquet-magic-number+ 0) (read-char stream)))
  (assert (char= #.(char +parquet-magic-number+ 1) (read-char stream)))
  (assert (char= #.(char +parquet-magic-number+ 2) (read-char stream)))
  (assert (char= #.(char +parquet-magic-number+ 3) (read-char stream)))
  t)

