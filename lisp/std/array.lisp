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

;; From Shinmera's PIPING
(defun array-shift (array &key (n 1) (from 0) (to (length array)) (adjust t) (fill nil f-p))
  "Shift a subset of array elements by a specified amount.
Optionally extend the array and fill empty space with a specified element.

N      --- The amount to be moved. Positive for right-shift, negative for left-shift.
FROM   --- region start point.
TO     --- region end point.
ADJUST --- Whether to adjust the fill pointer and the array bounds.
FILL   --- If provided, any empty space will be filled with this element."
  (when (and adjust (array-has-fill-pointer-p array))
    (unless (array-in-bounds-p array (+ (fill-pointer array) n))
      (adjust-array array (+ (fill-pointer array) n)))
    (incf (fill-pointer array) n))
  (if (< 0 n)
      (progn
        (loop repeat (- to from)
              for cursor downfrom (1- to)
              do (setf (aref array (+ cursor n))
                       (aref array cursor)))
        (when f-p
          (loop repeat n
                for cursor from from below to
                do (setf (aref array cursor) fill))))
      (progn
        (loop repeat (- to from)
              for cursor from (+ from n)
              do (setf (aref array cursor)
                       (aref array (- cursor n))))
        (when f-p
          (loop repeat (- n)
                for cursor downfrom (1- to) to from
                do (setf (aref array cursor) fill)))))
  array)

(defun vector-push-extend-position (element vector position)
  "Push the element into the specified position and shift-right to make
space. This is potentially very costly as all elements after the given
position need to be shifted as per ARRAY-SHIFT."
  (array-shift vector :from position)
  (setf (aref vector position) element)
  (fill-pointer vector))

(defun vector-pop-position (vector position)
  "Pop the element at the given position off the vector and return it.
This is potentially very costly as all elements after the given position
need to be shifted back as per ARRAY-SHIFT."
  (let ((el (aref vector position)))
    (array-shift vector :n -1 :from (1+ position))
    el))
