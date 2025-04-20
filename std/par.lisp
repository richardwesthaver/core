;;; cognate.lisp --- Parallelized versions of Common Lisp functions

;; Based on LPARALLEL cognates

;;; Commentary:

;; ref: https://github.com/lmj/lparallel

;;; Code:
(in-package :std/par)

;;; Utils
(defun zip-vector (seqs)
  (apply #'map 'vector #'list seqs))

(defun find-min-length (seqs)
  (reduce #'min seqs :key #'length))

(defun subsize (seq size start end)
  (let ((ret (- (or end size) start)))
    (when (or (minusp ret) (> ret size))
      (error "Bad range for seq ~A: :start ~A :end ~A"
             seq start end))
    ret))

;;; Subdivide
(defun find-num-parts (size parts-hint)
  (multiple-value-bind (quo rem) (floor size parts-hint)
    (values (if (zerop quo) rem parts-hint) quo rem)))

(defmacro with-parts (seq-size parts-hint &body body)
  (with-gensyms (quo rem index num-parts part-offset part-size)
    `(multiple-value-bind
           (,num-parts ,quo ,rem) (find-num-parts ,seq-size ,parts-hint)
       (declare (fixnum ,num-parts ,quo ,rem))
       (let ((,index 0)
             (,part-offset 0)
             (,part-size 0))
         (declare (fixnum ,index ,part-offset ,part-size))
         (flet ((next-part ()
                  (when (< ,index ,num-parts)
                    (unless (zerop ,index)
                      (incf ,part-offset ,part-size))
                    (setf ,part-size (if (< ,index ,rem) (1+ ,quo) ,quo))
                    (incf ,index)))
                (part-size   () ,part-size)
                (part-offset () ,part-offset)
                (num-parts   () ,num-parts))
           (declare (inline part-size part-offset num-parts)
                    (ignorable #'part-size #'part-offset #'num-parts))
           ,@body)))))

(defun subdivide-array (array size parts-hint)
  (with-parts size parts-hint
    (map-into (make-array (num-parts))
              (lambda ()
                (next-part)
                (make-array (part-size)
                            :displaced-to array
                            :displaced-index-offset (part-offset)
                            :element-type (array-element-type array))))))

(defun subdivide-list (list size parts-hint)
  (with-parts size parts-hint
    (loop with p = list
          while (next-part)
          collect p
          do (setf p (nthcdr (part-size) p)))))

(defun subdivide-list/slice (list size parts-hint)
  (with-parts size parts-hint
    (loop with p = list
          while (next-part)
          collect p into firsts
          collect (prog1 (setf p (nthcdr (1- (part-size)) p))
                    (setf p (prog1 (cdr p) (setf (cdr p) nil)))) into lasts
          finally (return (values firsts
                                  (lambda ()
                                    ;; stitch it back together
                                    (loop for last  in lasts
                                          for first in (cdr firsts)
                                          do (setf (cdr last) first)
                                          finally (setf (cdr last) p))))))))

(defun make-parts (result size parts-hint &key slicep)
  (if (listp result)
      (funcall (if slicep #'subdivide-list/slice #'subdivide-list)
               result size parts-hint)
      (subdivide-array result size parts-hint)))

(defun make-result-parts (result size parts-hint)
  "Subdivide the result sequence. For a list, delineate boundaries by slicing."
  (make-parts result size parts-hint :slicep t))

(defun make-input-parts (sequences size parts-hint)
  "Subdivide and interleave sequences for parallel mapping."
  (zip-vector (mapcar (lambda (seq) (make-parts seq size parts-hint))
                      sequences)))

;;; Reduce
(defmacro with-preduce-context (size parts &body body)
  (with-gensyms (results)
    `(with-parts ,size ,parts
       (let ((,results (make-array (num-parts))))
         (with-submit-indexed (num-parts) ,results
           ,@body
           (receive-indexed))))))
