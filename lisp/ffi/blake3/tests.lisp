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

(define-constant +hash-nada+ 
  #.#(172 111 134 255 246 48 165 106 33 245 157 58
      12 28 105 7 254 63 124 175 213 250 145 111
      155 114 32 50 246 5 158 217 216 222 167 143
      206 61 15 99 72 99 208 90 38 246 101 122 222
      5 96 234 65 174 214 3 217 107 178 217 200 23
      252 67 150 128 229 143 112 46 57 25 59 172
      84 134 98 192 184 163 104 149 37 37 222 119
      219 107 68 245 186 136 225 140 151 142 145
      77 74 95)
  :test 'equalp)

(deftest common ()
  (let ((a (make-octets 100)))
    (with-alien ((h blake3-hasher)
                 (o (array (unsigned 8) 100))
                 (len size-t 100)
                 (dat (array unsigned-char 100)))
      (blake3-hasher-init (addr h))
      (blake3-hasher-update (addr h) (addr dat) len)
      (blake3-hasher-finalize (addr h) (cast o (* unsigned-char)) len)
      (blake3-hasher-reset (addr h))
      (isequalp +hash-nada+ (clone-octets-from-alien o a)))))

(deftest less-common ()
  (with-alien ((h blake3-hasher)
               (k (array unsigned-char 32))
               (ctx (array char 32))
               (out (array unsigned-char 32)))
    (blake3-hasher-init-keyed (addr h) k)
    (blake3-hasher-init-derive-key-raw (addr h) (cast ctx (* t)) 32)
    (blake3-hasher-finalize-seek (addr h) 2 (cast out (* unsigned-char)) 32)
    (is (null (blake3-hasher-finalize (addr h) (cast out (* unsigned-char)) 32)))))
