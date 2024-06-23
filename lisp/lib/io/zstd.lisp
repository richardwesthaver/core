;;; zstd.lisp --- Zstd IO API

;; High-level Zstd API

;;; Code:
(in-package :io/zstd)

(eval-always (deferror zstd-error (io-error) () (:auto t)))

(deferror zstd-input-error (zstd-error) () (:auto t))
(deferror zstd-output-error (zstd-error) () (:auto t))

(deferror zstd-checksum-error (zstd-error) () (:auto t))
(deferror zstd-dictionary-error (zstd-error) () (:auto t))

(defclass zstd-input ()
  ((input :initform (sb-alien:make-alien zstd:zstd-inbuffer) :type zstd:zstd-inbuffer)))

(defclass zstd-output ()
  ((output :initform (zstd::allocate-zstd-outbuffer) :type zstd:zstd-outbuffer)))

(defclass zstd-compressor (compressor zstd-input zstd-output)
  ((stream :initform nil :type (or null zstd:zstd-cstream))))

(defclass zstd-decompressor (decompressor zstd-input zstd-output)
  ((stream :initform nil :type (or null zstd::zstd-dstream))))

;;; Simple API
(defmacro with-zstd-output (sym buffer))

(defmacro with-zstd-input ((sym buffer) &body body))
