;;; bits.lisp --- Bit manipulation

;;; Commentary:

;; CMUCL doc: https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node132.html

;; quick primer: https://cp-algorithms.com/algebra/bit-manipulation.html

;;; Code:
(defun make-bits (length &rest args)
  (apply #'make-array length (nconc '(:element-type bit) args)))

;; (defbytes (float 16))
;;  (unsigned-byte 1 2 3 4 8 16 24 32 64 128)
;;  (signed-byte 2 3 4 8 16 24 32 64 128)
;;  (float 16 24 32 64 128))
