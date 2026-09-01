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
           :chromaprint-encode-fingerprint
           :with-chromaprint-ctx
           :*default-chromaprint-algorithm*))

(in-package :chromaprint)

(define-alien-loader chromaprint "/usr/lib/")

(define-alien-enum (chromaprint-algorithm)
  :test1 0
  :test2 1
  :test3 2
  :test4 3
  :test5 4)

(defvar *default-chromaprint-algorithm* (chromaprint-algorithm :test2))

(define-opaque chromaprint-context)
;; (define-opaque chromaprint-matcher-context)

(defar chromaprint-get-version c-string)

(defar chromaprint-new (* chromaprint-context) (algo int))

(defar chromaprint-free void (ctx (* chromaprint-context)))

(defar chromaprint-get-algorithm int (ctx (* chromaprint-context)))

(defar chromaprint-set-option int (ctx (* chromaprint-context)) (name c-string) (value int))

(defar chromaprint-get-num-channels int (ctx (* chromaprint-context)))

(defar chromaprint-get-sample-rate int (ctx (* chromaprint-context)))

(defar chromaprint-get-item-duration int (ctx (* chromaprint-context)))

(defar chromaprint-get-item-duration-ms int (ctx (* chromaprint-context)))

(defar chromaprint-get-delay int (ctx (* chromaprint-context)))

(defar chromaprint-get-delay-ms int (ctx (* chromaprint-context)))

(defar chromaprint-start int (ctx (* chromaprint-context)) (sample-rate int) (num-channels int))

(defar chromaprint-feed int (ctx (* chromaprint-context)) (data (* short)) (size int))

(defar chromaprint-finish int (ctx (* chromaprint-context)))

(defar chromaprint-get-fingerprint int (ctx (* chromaprint-context)) (fingerprint (* c-string)))

(defar chromaprint-get-raw-fingerprint int (ctx (* chromaprint-context)) (fingerprint (* (* unsigned-int))) (size (* int)))

(defar chromaprint-get-raw-fingerprint-size int (ctx (* chromaprint-context)) (size (* int)))

(defar chromaprint-get-fingerprint-hash int (ctx (* chromaprint-context)) (hash (* unsigned-int)))

(defar chromaprint-clear-fingerprint int (ctx (* chromaprint-context)))

(defar chromaprint-encode-fingerprint int 
  (fp (* unsigned-int)) 
  (size int)
  (algo int)
  (encoded-fp (* c-string))
  (encoded-size (* int))
  (base64 int))

(defar chromaprint-decode-fingerprint int 
  (encoded-fp c-string)
  (encoded-size int)
  (fp (* (* unsigned)))
  (size (* (* int)))
  (algo (* int))
  (base64 int))

(defar chromaprint-hash-fingerprint int
  (fp (* unsigned))
  (size int)
  (hash (* unsigned)))

(defar chromaprint-dealloc void (ptr (* t)))

;;; Utils
(defmacro with-chromaprint-ctx ((sym &key (algo :default)
                                          (samplerate 44100)
                                          (channels 2))
                                &body body)
  `(let ((,sym (chromaprint-new ,(if (eql algo :default) *default-chromaprint-algorithm* `(chromaprint-algorithm ,algo)))))
     (unwind-protect (progn
                       (chromaprint-start ,sym ,samplerate ,channels)
                       ,@body)
       (chromaprint-free ,sym))))
