;;; tests.lisp --- BLAS Tests

;;; Code:
(defpackage :blas/tests
  (:use :cl :log :std :rt :blas :sb-alien :io/static))
(in-package :blas/tests)
(defsuite :blas)
(in-suite :blas)
(load-blas)
(deftest sanity ()
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

(deftest static-vector ())
