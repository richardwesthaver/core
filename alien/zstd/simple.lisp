;;; simple.lisp --- Zstd Simple API

;; 

;;; Code:
(in-package :zstd)

(std:deferror zstd-alien-simple-error (zstd-alien-error std-error) () (:auto t))

(defar "ZSTD_compress" size-t
  (dst (* t))
  (dst-capacity size-t)
  (src (* t))
  (src-size size-t)
  (compression int))

(defar "ZSTD_decompress" size-t
  (dst (* t))
  (dst-capacity size-t)
  (src (* t))
  (compressed-size size-t))

(defun zstdc (octets &optional (level 3))
  (let* ((len (length octets))
         (clen (zstd-compressbound len)))
    (with-alien ((in (* (unsigned 8)) (make-alien (unsigned 8) len))
                 (out (* (unsigned 8)) (make-alien (unsigned 8) clen)))
      (clone-octets-to-alien octets in)
      (let ((csize (zstd-compress out clen in len level)))
        (if (= 1 (zstd-iserror csize))
            (zstd-alien-simple-error (zstd-geterrorname csize))
            (coerce
             (loop for i from 0 below csize
                   collect (deref out i))
             'octet-vector))))))

(defun zstdd (octets &optional (capacity 4096))
  (let ((len (length octets)))
    (with-alien ((in (* (unsigned 8)) (make-alien (unsigned 8) len)))
      (clone-octets-to-alien octets in)
      (with-alien ((out (* (unsigned 8)) (make-alien (unsigned 8) capacity)))
        (let ((dsize (zstd-decompress out capacity in len)))
          (if (= 1 (zstd-iserror dsize))
              (zstd-alien-simple-error (zstd-geterrorname dsize))
              (coerce
               (loop for i from 0 below dsize
                     collect (deref out i))
               'vector)))))))

;; (zstdd (zstdc (make-array 4000 :initial-element (random 255) :element-type 'integer) 22))
