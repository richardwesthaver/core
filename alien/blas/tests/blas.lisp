;;; tests.lisp --- BLAS Tests

;;; Code:
(defpackage :blas/tests
  (:use :cl :log :std :rt :blas :sb-alien :io/static))
(in-package :blas/tests)
(defsuite :blas)
(in-suite :blas)
(load-blas)

;;; Level 1
;; from CL-OPENBLAS
(defun test-saxpy (n &key (repeat 100))
  ;; (with-static-vectors
  (let ((alpha 1.f0)
        (x (make-array n :element-type 'single-float :initial-element 0.f0))
        (y (make-array n :element-type 'single-float :initial-element 0.f0))
        (incx 1)
        (incy 1))
    (sb-sys:with-pinned-objects (x y)
      (time
       (dotimes (i repeat)
         (blas::saxpy n alpha (float-array-pointer x) incx (float-array-pointer y) incy)))
      (isequalp x y))))

(defun lisp-saxpy (z a x y)
  "Compute BLAS Level 1 SAXPY operation: zi = a * xi + yi with map-into CL
procedure."
  (declare (type single-float a))
  (declare (type (simple-array single-float (*))
                 z x y))
  (declare (optimize (speed 3)
                     (compilation-speed 0)
                     (safety 0)
                     (debug 0)))
  (let ((f (lambda (xi yi)
             (+ (* a xi) yi))))
    (map-into z f x y)))

(defun test-lisp-saxpy (n &key (repeat 100))
  (let ((alpha 1.f0)
        (x (make-array n :element-type 'single-float :initial-element 0.f0))
        (y (make-array n :element-type 'single-float :initial-element 0.f0))
        (z (make-array n :element-type 'single-float :initial-element 0.f0)))
    (time
     (dotimes (i repeat)
       (lisp-saxpy z alpha x y)))
    (isequalp x y)))

#| saxpy
Evaluation took:
  0.486 seconds of real time
  3.775713 seconds of total run time (3.772633 user, 0.003080 system)
  776.95% CPU
  1,949,168,782 processor cycles
  31,872 bytes consed
|#
#| lisp-saxpy
Evaluation took:
  0.775 seconds of real time
  0.773612 seconds of total run time (0.773530 user, 0.000082 system)
  99.87% CPU
  3,111,793,188 processor cycles
  0 bytes consed
|#
;; (defbench saxpy ()
;;   (println :BLAS-SAXPY)
;;   (test-saxpy 10000000)
;;   (println :LISP-SAXPY)
;;   (test-lisp-saxpy 10000000))

;;; Level 2

;;; Level 3
(deftest dgemm ()
  (let ((a (make-array '(2 3) :element-type 'double-float
                              :initial-contents '((2d0 1d0 6d0) (7d0 3d0 4d0))))
        (b (make-array '(3 2) :element-type 'double-float
                              :initial-contents '((3d0 1d0) (6d0 5d0) (2d0 3d0))))
        (c (make-array '(2 2) :element-type 'double-float))
        (expected-c (make-array '(2 2) :element-type 'double-float
                                       :initial-contents '((24d0 25d0) (47d0 34d0)))))
    (sb-sys:with-pinned-objects (a b c)
      (dgemm (char-code #\n) (char-code #\n) 2 2 3 1d0 (double-array-pointer b) 2 (double-array-pointer a) 3 0d0 (double-array-pointer c) 2))
    (isequalp expected-c c)))

