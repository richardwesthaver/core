;;; magick.asd --- IMAGEMAGICK SYSTEMS

;; MAGICKWAND for lisp.

;;; Commentary:

;; you must build in MAGICK/C like so:

;; gcc -shared -O3 -o libmagick.so magick.c magick_dispatch.c magick_portable.c \
;;   magick_sse2_x86-64_unix.S magick_sse41_x86-64_unix.S magick_avx2_x86-64_unix.S \
;;   magick_avx512_x86-64_unix.S

;;; Code:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-grovel))

(defpackage :magick.sys
  (:use :cl :asdf :sb-grovel :sb-alien))

(in-package :magick.sys)

(defsystem "magick"
  :description "MAGICK/C FFI"
  :depends-on (:sb-grovel :std)
  :in-order-to ((test-op (test-op "magick/tests")))
  :components ((:file "pkg")
               (grovel-constants-file "constants"
                                      :package :magick))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :magick)))

(defsystem "magick/tests"
  :depends-on (:rt :magick)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests :magick)))
