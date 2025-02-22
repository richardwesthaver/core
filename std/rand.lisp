;;; rand.lisp --- Random Utils

;; 

;;; Code:
(in-package :std/rand)

;;; random
(defvar *simple-charset* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

(defun random-elt (seq)
  (elt seq (random (length seq))))

(defun random-ref (vec)
  (aref vec (random (length vec))))

(defun random-char ()
  (random-ref *simple-charset*))

(defun random-chars (dim)
  (let ((r (make-array dim :element-type 'character)))
    (dotimes (i (array-total-size r) r)
      (setf (row-major-aref r i) (random-char)))))

(defun random-byte () (random 255))

(defun random-bytes (dim)
  (let ((r (make-array dim :element-type 'octet)))
    (dotimes (i (array-total-size r) r)
      (setf (row-major-aref r i) (random-byte)))))

