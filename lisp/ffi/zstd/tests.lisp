;;; zstd/tests.lisp --- Zstd FFI tests

;;

;;; Code:
(defpackage :zstd/tests 
    (:use :cl :std :rt :zstd))

(in-package :zstd/tests)

(defsuite :zstd)
(in-suite :zstd)

(load-zstd)

(deftest sanity ()
  (mapc (lambda (x)
          (is (= (car x) (cdr x))))
        `((,zstd::zstd-clevel-default . 3)
          (,zstd::zstd-magicnumber . -47205080)
          (,zstd::zstd-magic-skippable-start . 407710288)
          (,zstd::zstd-blocksizelog-max . 17)
          (,zstd::zstd-blocksize-max . 131072)
          (,zstd::zstd-contentsize-unknown . -1)
          (,zstd::zstd-contentsize-error . -2)
          (,zstd::zstd-max-input-size . -71777214294589696)
          (,zstd::zstd-version-number . (zstd::zstd-versionnumber))
          (,zstd::zstd-magic-dictionary . -332356553))))

(deftest simple ()
  ;; (zstd::zstd-compress)
  ;; (zstd::zstd-decompress)
)

(deftest cstream ()
  (let ((ret 0)
        (in (zstd::allocate-zstd-inbuffer-s))
        (out (zstd::allocate-zstd-outbuffer-s))
        (cst (zstd::zstd-createcstream)))
    (setf ret (zstd::zstd-initcstream cst 9))
    (is (= 0 (zstd::zstd-iserror ret)))
    (zstd::zstd-compressstream cst out in)
    (is (= 0 (zstd::zstd-compressstream2 cst out in 0)))))
