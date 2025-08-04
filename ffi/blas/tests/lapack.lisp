;;; tests/lapack.lisp --- LAPACK Tests

;;; Code:
(defpackage :lapack/tests
  (:use :cl :log :std :rt :lapack :sb-alien :io/static))
(in-package :lapack/tests)
(defsuite :lapack)
(in-suite :lapack)
(blas:load-lapack)
(deftest sanity ())
