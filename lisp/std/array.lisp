;;; std/array.lisp --- Standard Arrays

;;

;;; Code:
;; sb-kernel:with-array-data
(in-package :std/array)

(defun copy-array (array)
  (let ((new-array
          (make-array (array-dimensions array)
                      :element-type (array-element-type array)
                      :adjustable (adjustable-array-p array)
                      :fill-pointer (and (array-has-fill-pointer-p array)
                                         (fill-pointer array)))))
    (loop for i below (array-total-size array)
          do (setf (row-major-aref new-array i)
                   (row-major-aref array i)))
    new-array))

(deftype signed-array-length ()
  "A (possibly negated) array length."
  '#.(let ((limit (1- array-dimension-limit)))
       `(integer ,(- limit) ,limit)))
