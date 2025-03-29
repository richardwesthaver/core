;;; pkg.lisp --- low-level bindings to CHROMAPRINT

;;; Commentary:

#|
Chromaprint is a library for generating audio fingerprints, mainly to be used with the <a href="https://acoustid.org">AcoustID</a> service.

It needs raw audio stream (16-bit signed int) on input. The audio can have any
sampling rate and any number of channels. Typically, you would use some native
library for decoding compressed audio files and feed the result into
Chromaprint.  Audio fingerprints returned from the library can be represented
either as base64-encoded strings or 32-bit integer arrays. The base64-encoded
strings are usually what's used externally when you need to send the
fingerprint to a service. You can't directly compare the fingerprints in such
form.  The 32-bit integer arrays are also called "raw fingerprints" and they
represent the internal structure of the fingerprints. If you want to compare
two fingerprints yourself, you probably want them in this form.  |#
;;; Code:
(defpackage :chromaprint
  (:use :cl :std :sb-alien)
  (:export :load-chromaprint
           :chromaprint-hash-fingerprint
           :chromaprint-decode-fingerprint
           :chromaprint-dealloc
           :chromaprint-algorithm
           :chromaprint-context
           :chromaprint-get-version
           :chromaprint-new
           :chromaprint-free
           :chromaprint-get-algorithm
           :chromaprint-set-option
           :chromaprint-get-num-channels
           :chromaprint-get-sample-rate
           :chromaprint-get-item-duration
           :chromaprint-get-item-duration-ms
           :chromaprint-get-delay
           :chromaprint-get-delay-ms
           :chromaprint-start
           :chromaprint-feed
           :chromaprint-finish
           :chromaprint-get-fingerprint
           :chromaprint-get-raw-fingerprint
           :chromaprint-get-raw-fingerprint-size
           :chromaprint-get-fingerprint-hash
           :chromaprint-clear-fingerprint
           :chromaprint-encode-fingerprint))

(in-package :chromaprint)

(define-alien-loader chromaprint "/usr/lib/")

(define-alien-enum (chromaprint-algorithm int)
  :test1 0
  :test2 1
  :test3 2
  :test4 3
  :test5 4
  :default 1)

(define-opaque chromaprint-context)
;; (define-opaque chromaprint-matcher-context)

(define-alien-routine chromaprint-get-version c-string)

(define-alien-routine chromaprint-new (* chromaprint-context) (algo int))

(define-alien-routine chromaprint-free void (ctx (* chromaprint-context)))

(define-alien-routine chromaprint-get-algorithm int (ctx (* chromaprint-context)))

(define-alien-routine chromaprint-set-option int (ctx (* chromaprint-context)) (name c-string) (value int))

(define-alien-routine chromaprint-get-num-channels int (ctx (* chromaprint-context)))

(define-alien-routine chromaprint-get-sample-rate int (ctx (* chromaprint-context)))

(define-alien-routine chromaprint-get-item-duration int (ctx (* chromaprint-context)))

(define-alien-routine chromaprint-get-item-duration-ms int (ctx (* chromaprint-context)))

(define-alien-routine chromaprint-get-delay int (ctx (* chromaprint-context)))

(define-alien-routine chromaprint-get-delay-ms int (ctx (* chromaprint-context)))

(define-alien-routine chromaprint-start int (ctx (* chromaprint-context)) (sample-rate int) (num-channels int))

(define-alien-routine chromaprint-feed int (ctx (* chromaprint-context)) (data (* short)) (size int))

(define-alien-routine chromaprint-finish int (ctx (* chromaprint-context)))

(define-alien-routine chromaprint-get-fingerprint int (ctx (* chromaprint-context)) (fingerprint (* c-string)))

(define-alien-routine chromaprint-get-raw-fingerprint int (ctx (* chromaprint-context)) (fingerprint (* (array unsigned-int))) (size (* int)))

(define-alien-routine chromaprint-get-raw-fingerprint-size int (ctx (* chromaprint-context)) (size (* int)))

(define-alien-routine chromaprint-get-fingerprint-hash int (ctx (* chromaprint-context)) (hash (* unsigned-int)))

(define-alien-routine chromaprint-clear-fingerprint int (ctx (* chromaprint-context)))

(define-alien-routine chromaprint-encode-fingerprint int 
  (fp (* unsigned-int)) 
  (size int)
  (algo int)
  (encoded-fp (* c-string))
  (encoded-size (* int))
  (base64 int))

(define-alien-routine chromaprint-decode-fingerprint int 
  (encoded-fp c-string)
  (encoded-size int)
  (fp (* (* unsigned)))
  (size (* (* int)))
  (algo (* int))
  (base64 int))

(define-alien-routine chromaprint-hash-fingerprint int
  (fp (* unsigned))
  (size int)
  (hash (* unsigned)))

(define-alien-routine chromaprint-dealloc void (ptr (* t)))
