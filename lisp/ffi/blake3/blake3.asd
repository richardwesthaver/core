;;; blake3.asd --- BLAKE3 SYSTEMS

;; BLAKE3 for lisp.

;;; Commentary:

;; you must build in BLAKE3/C like so:

;; gcc -shared -O3 -o libblake3.so blake3.c blake3_dispatch.c blake3_portable.c \
;;   blake3_sse2_x86-64_unix.S blake3_sse41_x86-64_unix.S blake3_avx2_x86-64_unix.S \
;;   blake3_avx512_x86-64_unix.S

;;; Code:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-grovel))

(defpackage :blake3.sys
  (:use :cl :asdf :sb-grovel :sb-alien))

(in-package :blake3.sys)

(defsystem "blake3"
  :description "BLAKE3/C FFI"
  :depends-on (:sb-grovel :std)
  :in-order-to ((test-op (test-op "blake3/tests")))
  :components ((:file "pkg")
               (grovel-constants-file "constants"
                                      :package :blake3))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :blake3)))

(defsystem "blake3/tests"
  :depends-on (:std/rt :blake3)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :blake3)))
