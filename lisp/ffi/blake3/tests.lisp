;;; k/tests.lisp --- k tests

;;; Code:
(defpackage :blake3/tests
  (:use :cl :std :std/rt :std/fu :blake3 :std/alien :sb-ext))

(in-package :blake3/tests)

(defsuite :blake3)
(in-suite :blake3)

(load-blake3)

(deftest version ()
  (is (string= "1.5.0" (blake3-version))))

(deftest hasher ()
  (with-alien ((h blake3-hasher)
               (o (* (unsigned 8)))
               (olen size-t))
    (blake3-hasher-init (addr h))
    (blake3-hasher-update (addr h) nil 0)
    (blake3-hasher-finalize (addr h) o olen)
    (print (addr h))
    (print (addr o))
    (blake3-hasher-reset (addr h))))
