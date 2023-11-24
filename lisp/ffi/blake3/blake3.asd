;;; blake3.asd --- BLAKE3 SYSTEMS

;; BLAKE3 for lisp.

;;; Commentary:

;; you must build in BLAKE3/C like so:

;; gcc -shared -O3 -o libblake3.so blake3.c blake3_dispatch.c blake3_portable.c \
;;   blake3_sse2_x86-64_unix.S blake3_sse41_x86-64_unix.S blake3_avx2_x86-64_unix.S \
;;   blake3_avx512_x86-64_unix.S

;;; Code:
(defsystem "blake3"
  :description "BLAKE3/C FFI"
  :defsystem-depends-on (:asdf-package-system)
  :class :package-inferred-system
  :depends-on (:std :blake3/pkg)
  :in-order-to ((test-op (test-op "blake3/tests")))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :blake3)))
