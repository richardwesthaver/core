;;; zlib.lisp --- ZLIB Compression

;; 

;;; Code:
(in-package :io/zlib)
(deferror zlib-error (flate-error io-error) () (:auto t))

(defclass zlib-compressing-stream (compressing-stream) ())

(defclass zlib-decompressing-stream (decompressing-stream) ())

;; TODO 2025-05-12: 
(defmacro with-zlib-input ())
(defmacro with-zlib-output ())
(defmacro with-zlib-buffer ())
