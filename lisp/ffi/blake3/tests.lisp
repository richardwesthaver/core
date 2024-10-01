;;; k/tests.lisp --- k tests

;;; Code:
(defpackage :blake3/tests
  (:use :cl :std :rt :blake3 :sb-ext :sb-alien))

(in-package :blake3/tests)

(defsuite :blake3)
(in-suite :blake3)

(load-blake3)

(deftest version ()
  (is (stringp (blake3-version))))

(deftest common ()
  (with-alien ((h blake3-hasher)
               (o (* (unsigned 8)))
               (olen size-t))
    (blake3-hasher-init (addr h))
    (blake3-hasher-update (addr h) nil 0)
    (blake3-hasher-finalize (addr h) o olen)
    (blake3-hasher-reset (addr h))))

(deftest less-common ()
  (with-alien ((h blake3-hasher)
               (k (array unsigned-char 32))
               (ctx (array char 32))
               (out (array unsigned-char 32)))
    (blake3-hasher-init-keyed (addr h) k)
    (blake3-hasher-init-derive-key-raw (addr h) (cast ctx (* t)) 32)
    (blake3-hasher-finalize-seek (addr h) 2 (cast out (* unsigned-char)) 32)
    (is (null (blake3-hasher-finalize (addr h) (cast out (* unsigned-char)) 32)))))
    
