;;; sfc.lisp --- Space Filling Curves

;; 

;;; Code:
(in-package :math/sfc)

;; recursive
(defun hilbert-list (n)
  (unless (zerop n)
    (let (points)
      (labels ((recur (x y lg i1 i2)
                 (if (= lg 1)
                     (push (cons x y) points)
                     (let ((lg (floor lg 2)))
                       (recur (+ x (* i1 lg))       (+ y (* i1 lg))       lg i1       (- 1 i2))
                       (recur (+ x (* i2 lg))       (+ y (* (- 1 i2) lg)) lg i1       i2)
                       (recur (+ x (* (- 1 i1) lg)) (+ y (* (- 1 i1) lg)) lg i1       i2)
                       (recur (+ x (* (- 1 i2) lg)) (+ y (* i2 lg))       lg (- 1 i1) i2)))))
        (recur 0 0 (expt n 2) 0 0))
      (nreverse points))))

;; REVIEW 2025-05-10: something interesting is happening here on odd generations - kinda interesting
(defun hilbert-curve (&optional (n 8))
  "Draw one of Hilbert's continuous fractal space-filling curves."
  (let* ((points (hilbert-list n))
         (dim (expt n 2))
        (grid (make-array (list dim dim))))
    (let ((start (calculate-box-graphic (first points) (second points) (third points) t)))
      (setf (aref grid 0 0) start
            (aref grid (1- dim) 0) start))
    (loop for (from to next) on points
          while next
          do (setf (aref grid (car to) (cdr to))
                   (calculate-box-graphic from to next)))
    (loop for y from 0 to (1- dim)
          do (loop for x from 0 to (1- dim)
                   do (princ (aref grid x y)))
             (fresh-line))))

(defun calculate-box-graphic (from to next &optional start)
  (flet ((direction (from to)
           (flet ((x (loc) (car loc))
                  (y (loc) (cdr loc)))
             (cond
               ((< (x from) (x to)) :left)
               ((> (x from) (x to)) :right)
               ((< (y from) (y to)) :up)
               ((> (y from) (y to)) :down)))))
    (let ((in (direction from to))
          (out (direction next to)))
      (if start
        (ecase in
          (:up #\BOX_DRAWINGS_LIGHT_VERTICAL)
          (:left #\BOX_DRAWINGS_LIGHT_HORIZONTAL))
        (second (assoc-if (lambda (el)
                            (or (and (eq (first el) in)
                                     (eq (second el) out))
                                (and (eq (first el) out)
                                     (eq (second el) in))))
                          '(((:up    :right) #\BOX_DRAWINGS_LIGHT_UP_AND_RIGHT)
                            ((:up    :left)  #\BOX_DRAWINGS_LIGHT_UP_AND_LEFT )
                            ((:down  :right) #\BOX_DRAWINGS_LIGHT_DOWN_AND_RIGHT)
                            ((:down  :left)  #\BOX_DRAWINGS_LIGHT_DOWN_AND_LEFT)
                            ((:left  :right) #\BOX_DRAWINGS_LIGHT_HORIZONTAL)
                            ((:up    :down)  #\BOX_DRAWINGS_LIGHT_VERTICAL))))))))
