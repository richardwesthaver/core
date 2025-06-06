(defpackage :io/tests
  (:use :cl :std :rt :io :uring :zstd :sb-gray :disk :disk/btrfs :io/stream :io/deflate))

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

(deftest streams ()
  "IO/STREAM tests"
  ;; bound
  ;; peeking
  ;; buffer? currently in dat/serde
  (istype 'bound-input-stream (make-instance 'bound-input-stream))
  ;; (make-instance 'peeking-input-stream :stream (nyi!))
  )

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
    (istype 'chunked-io-stream (make-chunked-stream (make-two-way-stream input output)))
    (istype 'blocked-input-stream (make-instance 'blocked-input-stream))))

(defparameter *data-size* (* 10 1024))

(deftest zstd-simple ()
  (let ((data (make-array *data-size* :element-type 'octet :initial-contents (random-bytes *data-size*)))
        (round-trip-data (make-octets *data-size*))
        compressed-data)
    (setf compressed-data
          (with-zstd-buffer (b data :direction :output) b))
    (setf round-trip-data
          (with-zstd-buffer (b compressed-data :direction :input) b))
    (is (equalp round-trip-data data))))

;; FIX 2025-03-27: 
(deftest zstd-stream ()
  (let* ((bsize 4096)
         (ssize (* 20 bsize))
         (data (make-octets ssize :initial-contents (random-bytes ssize)))
         (compressor (make-instance 'zstd-compressor))
         (decompressor (make-instance 'zstd-decompressor)))
    (unwind-protect
         (progn
           (loop for x below (/ ssize bsize)
                 with i = (* x bsize)
                 with v = (subseq data i (+ i bsize))
                 do (compress-with compressor v))
	   (finish-output compressor) ;; endstream
	   ;; (stream-force-output compressor) ;; flush
	   ;; (setf (output-size compressor) (output-position compressor))
                 ;; (output-position compressor) 0)
           (log:info! :in.pos (input-position compressor)
                      :in.size (input-size compressor)
                      :out.pos (output-position compressor)
                      :out.size (output-size compressor))
           (let ((compressed (make-array (output-position compressor) :element-type 'octet))
                 (decompressed (make-array (output-size compressor) :element-type 'octet)))
             (clone-octets-from-alien (output-buffer compressor) compressed)
             (println compressed)
             (decompress-with decompressor compressed)
             (clone-octets-from-alien
              (output-buffer decompressor)
              decompressed)
             (log:info! data)
             (log:info! decompressed)
             (is (equalp data decompressed))))
      ;; (close (stream-of decompressor))
      ;; (close (stream-of compressor))
      )))


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

;;; Deflate
(deftest gzip ())
(deftest bzip2 ())
(deftest zlib ())

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

(deftest disk-btrfs ()
  (is (load-filesystem-backend :btrfs))
  (let ((disk (make-instance 'btrfs-disk)))
    (issubclass 'disk (class-of disk)))
  ;; will return NIL on non-btrfs file systems
  (islist (btrfs-subvolumes "/tmp")))
