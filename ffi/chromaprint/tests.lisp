;;; chromaprint/tests.lisp --- Chromaprint tests

;;; Code:
(defpackage :chromaprint/tests
  (:use :cl :std :rt :chromaprint :sb-ext :sb-alien))

(in-package :chromaprint/tests)

(defsuite :chromaprint)
(in-suite :chromaprint)

(load-chromaprint)

(deftest sanity () (istype 'string (chromaprint-get-version)))

(deftest chroma-null () 
  (let ((ctx (chromaprint-new 1))
        (sr 44100)
        (n 2))
    (with-alien ((fp c-string))
      (chromaprint-start ctx sr n)
      (chromaprint-finish ctx)
      (chromaprint-get-fingerprint ctx (addr fp))
      (isequal "AQAAAA" fp)
      (chromaprint-free ctx))))


