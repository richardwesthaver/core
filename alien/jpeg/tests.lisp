;;; tests.lisp --- JPEG FFI Tests

;; 

;;; Code:
(defpackage :jpeg/tests
  (:use :cl :std :sb-alien :rt :jpeg))
(in-package :jpeg/tests)
(defsuite :jpeg)
(in-suite :jpeg)
(load-jpeg)
(load-turbojpeg)
(defvar *test-file* (system-relative-pathname :core ".stash/egypt.jpg"))
(deftest load-image () 
  (istype '(alien (* unsigned-char)) (load-jpeg-image *test-file* (make-instance 'jpeg-decompressor))))

(deftest save-image ()
  (time
   (let ((path (tmpize-pathname "save.jpg")))
     (multiple-value-bind (buf w h fmt size) (load-jpeg-image *test-file* (make-instance 'jpeg-decompressor))
       (save-jpeg-image 
        path buf w h 
        (make-instance 'jpeg-compressor :quality 1 :subsampling :440) :size size))
     (delete-file path))))

(deftest transform-image (:skip t)
  (transform-jpeg-image *test-file* (tmpize-pathname "transform.jpg") 0 (make-instance 'jpeg-transformer)))
