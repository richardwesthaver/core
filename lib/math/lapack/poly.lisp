;;; poly.lisp --- Polyfit

;; 

;;; Code:
(in-package :math/lapack)

(defun polyfit (observations &optional n &aux (observations (coerce observations 'vector)))  
  (let* ((n (or n (1- (length observations))))
         (A (zeros (list (length observations) (1+ n))))
         (b (zeros (dimensions A 0))))
    (labels ((coeff (n k)
               (if (< n k) 0 (loop with i = 1
                                   for jj 
                                   from n downto (- n k -1) 
                                   do (setf i (* i jj))
                                   finally (return i))))
             (row-ti (ti x &optional (derivative 0) &aux (pti 1d0))
               (loop for i from 0 below (dimensions x 0)
                     do (setf (ref x i) (* (coeff i derivative) pti))
                     if (<= derivative i) 
                     do (setf pti (* pti ti)))))
      (loop for li across observations
            for (Ai bi) being the slice of (list A b) along 0
            ;; FIX 2025-12-27: 
               do
               (match li
                 ((ti value &optional derivative)
                  (setf (ref bi 0) value)
                  (row-ti ti Ai (or derivative 0)))))
      (lstsq A b))))

(defun polyval (tt poly &aux (tn 1))
  (loop for i from 0 below (dimensions poly 0)
        summing (* (ref poly i) tn)
        do (setf tn (* tn tt))))

(defun roots (poly &aux (n (1- (dimensions poly 0))))
  ;;TODO: Add a better method.
  (let ((A (zeros (list n n) (type-of poly))))
    (if (< 1 n) (copy! 1 (diag~ A 1)))
    (scal! (/ -1 (ref poly -1)) (copy! (subtensor~ poly '((0 -1))) (subtensor~ A '(-1 (nil nil)))))
    (eig A :nn)))
