;;; tests/lapack.lisp --- LAPACK Tests

;;; Code:
(defpackage :lapack/tests
  (:use :cl :log :std :rt :lapack :sb-alien :io/static))
(in-package :lapack/tests)
(defsuite :lapack)
(in-suite :lapack)
(blas:load-lapack)
(deftest dlapy3 ()
  (let ((x (make-array '(2 3) :element-type 'double-float
                              :initial-contents '((2d0 1d0 6d0) (7d0 3d0 4d0)))))
    (with-alien ((n int 10)
                 (alph double 0.5d0)
                 (incx int 4)
                 (tau double 0d0))
      (sb-sys:with-pinned-objects (x)
        (lapack::dlarfg
         n
         (addr alph)
         (double-array-pointer x)
         incx
         (addr tau))
        (is> tau 1)))))
