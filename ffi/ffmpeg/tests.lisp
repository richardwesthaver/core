;;; tests.lisp --- FFmpeg FFI Tests

;; 

;;; Code:
(defpackage :ffmpeg/tests
  (:use :cl :std :sb-alien :rt :ffmpeg))
(in-package :ffmpeg/tests)

(defsuite :ffmpeg)
(in-suite :ffmpeg)

(load-avutil)
(load-avcodec)
(load-avformat)
(load-avfilter)

(deftest sanity ()
  (mapcar (lambda (i) (istype 'integer i)) 
          (list
           (avcodec-version)
           (avformat-version)
           (avutil-version)
           (avfilter-version))))

(deftest decode ()
  (let ((enc (avcodec-find-encoder (av-codec-id :h261))))
    (isnt (zerop (av-codec-is-encoder enc)))
    (is (zerop (av-codec-is-decoder enc)))
    (with-alien ((ctx (* av-codec-context) (avcodec-alloc-context3 enc)))
      

