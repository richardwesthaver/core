(defpackage :io/tests
  (:use :cl :std :rt :io :uring :zstd :sb-gray :disk :disk/btrfs))

(in-package :io/tests)
(defsuite :io)
(in-suite :io)

(load-uring)
(load-zstd)
(deftest sanity ()
  (uring::io-uring-major-version))

(deftest serve-event ()
  "See 'tests/serve-event.pure.lisp'"
  nil)

(deftest chunky ()
  "Tests for CHUNKED-STREAM"
  (let ((input (make-chunked-stream 
                (make-instance 'fundamental-binary-input-stream)))
        (output (make-chunked-stream 
                 (make-instance 'fundamental-binary-output-stream))))
    (istype 'chunked-stream 
            (make-chunked-stream 
             (make-instance 'fundamental-binary-stream)))
    (istype 'chunked-input-stream input)
    (istype 'chunked-output-stream output)
    (istype 'chunked-io-stream (make-chunked-stream (make-two-way-stream input output)))))

(defparameter *data-size* (* 10 1024))

(deftest zstd-simple ()
  (let ((data (make-array *data-size* :element-type '(unsigned-byte 8)
                                      :initial-contents (random-bytes *data-size*)))
        (round-trip-data (make-array *data-size* :element-type '(unsigned-byte 8)
                                                 :initial-element 0))
        compressed-data)
    (setf compressed-data 
          (io/zstd:with-zstd-buffer :output (out data)))
    (setf round-trip-data
          (io/zstd:with-zstd-buffer :input (in compressed-data)))
    (is (equalp round-trip-data data))))

(deftest zstd-stream ()
  (let* ((bsize 4096)
         (ssize (* 20 bsize))
         (data (make-octets ssize :initial-contents (random-bytes ssize)))
         (compressor (make-instance 'zstd-compressor))
         (decompressor (make-instance 'zstd-decompressor)))
    (unwind-protect
         (let ((outlen (reduce '+ 
                               (loop for x below (/ ssize bsize)
                                     with i = (* x bsize)
                                     with v = (subseq data i (+ i bsize))
                                     collect (compress-with compressor v)))))
           (force-output compressor)
           (finish-output compressor)
           (log:info! (input-position compressor)
                      (output-position compressor))
           (let ((compressed (make-octets outlen :adjustable t))
                 (decompressed (make-octets ssize)))
             (clone-octets-from-alien (output-buffer compressor) compressed outlen)
             (decompress-with decompressor compressed)
             ;; (clone-octets-from-alien 
             ;;  (output-buffer decompressor)
             ;;  decompressed)
             (is (equalp data decompressed))))
      (close (stream-of decompressor))
      (close (stream-of compressor)))))

#| test from salza2
(defparameter *data-size* (* 10 1024))

(define-test compressing-stream
"Test the compressing stream by round tripping random data through salza2 and
then chipz."
(let ((data (make-array *data-size* :element-type '(unsigned-byte 8)
:initial-contents (loop :repeat *data-size*
:collect (random 256))))
(round-trip-data (make-array *data-size* :element-type '(unsigned-byte 8)
:initial-element 0))
compressed-data)
(setf compressed-data
(flexi-streams:with-output-to-sequence (wrapped-stream)
(with-open-stream
(out-stream (salza2:make-compressing-stream 'salza2:gzip-compressor wrapped-stream))
(write-sequence data out-stream))))
(flexi-streams:with-input-from-sequence (wrapped-stream compressed-data)
(with-open-stream
(in-stream (chipz:make-decompressing-stream 'chipz:gzip wrapped-stream))
(read-sequence round-trip-data in-stream)
(is eql :eof (read-byte in-stream nil :eof))))
(is equalp data round-trip-data)))

(define-test compressing-stream-closed-error
(flexi-streams:with-output-to-sequence (wrapped-stream)
(let ((out-stream (salza2:make-compressing-stream 'salza2:gzip-compressor wrapped-stream)))
(write-byte 1 out-stream)
(close out-stream)
(fail (write-byte 2 out-stream) 'salza2:stream-closed-error))))
|#

;;; Static Vectors
(deftest static-vector ()
  (with-static-vector (v 4)
    (isequalp #(0 0 0 0) v))
  (isequalp #(0 0 0 0) (make-static-vector 4)))

;;; Smart Buffers
(deftest smart-buffer ()
  (let ((sb (make-smart-buffer)))
    (istype 'smart-buffer sb)))

;;; XSubseq
(deftest xsubseq ()
  (istype 'string
          (with-xsubseqs (ret)
            (iszero (xlength ret))
            (xnconcf ret (xsubseq "test" 0))
            (is= 4 (xlength ret)))))

;;; Disk
(deftest disk-generic ()
  (let ((disk (make-instance 'disk)))
    (istype 'disk disk)))

(deftest disk-btrfs (:skip (not (std:sudo-p)))
  (is (load-filesystem-backend :btrfs))
  (let ((disk (make-instance 'btrfs-disk)))
    (issubclass 'disk (class-of disk))))


