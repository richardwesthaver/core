(defpackage :io/tests
  (:use :cl :std :rt :io :uring))
(in-package :io/tests)
(defsuite :io)
(in-suite :io)

(load-uring)

(deftest sanity ()
  (uring::io-uring-major-version))

(deftest serve-event ()
  "See 'tests/serve-event.pure.lisp'."
  nil)

(defparameter *data-size* (* 10 1024))

(deftest zstd-simple ()
  (let ((data (make-array *data-size* :element-type '(unsigned-byte 8)
                                      :initial-contents (random-bytes *data-size*)))
        (round-trip-data (make-array *data-size* :element-type '(unsigned-byte 8)
                                                 :initial-element 0))
        compressed-data)
    (setf compressed-data 
          (io/zstd:with-zstd :output (out data1)))
    (setf round-trip-data
          (io/zstd:with-zstd :input (in compressed-data)))
    (is (equalp round-trip-data data))))

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
