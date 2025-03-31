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

(deftest chroma-random ()
  (let ((sr 44100) (n 2))
    (with-alien ((fp c-string))
      (io/static:with-static-vector (data #1=(* n sr) :initial-element (random 255))
        (with-chromaprint-ctx (ctx :samplerate sr :channels n)
	  (chromaprint-feed ctx (io/static:static-vector-pointer data) #1#)
	  (chromaprint-feed ctx (io/static:static-vector-pointer data) #1#)
	  (chromaprint-feed ctx (io/static:static-vector-pointer data) #1#)
          (chromaprint-finish ctx)
          (chromaprint-get-fingerprint ctx (addr fp))
          (is> (length fp) 6))))))
