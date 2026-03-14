(defpackage :io/tests
  (:use :cl :std :rt :io :uring :zstd :sb-gray :disk :disk/btrfs :io/stream :io/deflate :kbd
    :io/mux :io/sys))

(in-package :io/tests)
(defsuite :io)
(in-suite :io)

(load-uring)
(load-zstd)
(btrfs:load-btrfs)
(deftest sanity () (is (positive-integer-p (uring::io-uring-major-version))))

;; (deftest uring-serve-event ()
;;   "See 'tests/serve-event.pure.lisp'"
;;   nil)

(deftest streams ()
  "IO/STREAM tests"
  ;; bound
  ;; peeking
  ;; buffer? currently in dat/serde
  (istype 'bound-input-stream (make-instance 'bound-input-stream))
  (with-input-from-string (s "foobarbaz")
    (isequal "fo" (concatenate 'string (peeked (make-instance 'peeking-input-stream :stream s :count 2))))))

(deftest chunky ()
  "Tests for CHUNKED-STREAM"
  (let ((input (make-chunked-stream 
                (make-instance 'fundamental-binary-input-stream)))
        (output (make-chunked-stream 
                 (make-instance 'fundamental-binary-output-stream))))
    (istype 'chunked-io-stream
            (make-chunked-stream 
             (make-instance 'fundamental-binary-stream)))
    (istype 'chunked-input-stream input)
    (istype 'chunked-output-stream output)
    (istype 'chunked-io-stream (make-chunked-stream (make-two-way-stream input output)))
    (istype 'blocked-input-stream (make-instance 'blocked-input-stream))))

(defparameter *data-size* (ash 1024 4))

(deftest zstd-buffer ()
  (let ((data (make-array *data-size* :element-type 'octet :initial-contents (random-bytes *data-size*)))
        (round-trip-data (make-octets *data-size*))
        compressed-data)
    (setf compressed-data
          (with-zstd-buffer (b data :direction :output) b))
    (setf round-trip-data
          (with-zstd-buffer (b compressed-data :direction :input) b))
    (is (equalp round-trip-data data))))

;; FIX 2025-03-27: 
(deftest zstd-stream (:skip :todo)
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

;;; Deflate
(deftest gzip-stream (:skip :todo)
  "Test the compressing stream by round tripping random data."
  (let ((data (make-array *data-size* :element-type '(unsigned-byte 8)
                                              :initial-contents (loop repeat *data-size*
                                                                      collect (random 256))))
        (round-trip-data (make-array *data-size* :element-type '(unsigned-byte 8)
                                                         :initial-element 0))
        compressed-data)
    (setf compressed-data
          (with-output-to-string (s)
            (let ((c (make-compressing-stream 'gzip-compressor s)))
              (write-sequence data c))))
    (with-input-from-string (s compressed-data)
      (with-open-stream (in-stream (make-decompressing-stream :gzip s))
        (io/flate:decompress-with round-trip-data in-stream)
        (iseql :eof (read-byte in-stream nil :eof))))
    (isequalp data round-trip-data)))

(deftest gzip-stream-closed-error ()
  (let ((out-stream (make-compressing-stream 'gzip-compressor nil)))
    (close out-stream)
    (signals error (write-byte 2 out-stream))))

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

;;; KBD
(deftest keys ()
  (load-kbd-libs)
  (is= 99 (key-sym (kbd "C-c")))
  (isequalp '(99) (keysyms-from-character #\c))
  (is (key-control (make-key :control t)))
  (isnt (key-shift (make-key :sym 67)))
  (is (key-shift (make-key :sym 99 :shift t :control t :altgr t)))
  (is= 4 (length (apply 'cons (multiple-value-list (kbd "C-c t S-f z")))))
  (is= 99 (key-sym (parse-key "C-c")))
  (is= 5 (length (parse-keyseq "C-c f z S-1 C-u"))))

(deftest keymaps ()
  (istype 'keymap (sparse-keymap)))

(deftest lzw (:skip :todo))

(deftest sys ()
  (iseql 'minusp (io/sys::syscall-error-predicate #.(parse-alien-type 'int nil))))

(deftest mux ()
  (with-event-base (e)
    (let ((cb nil))
      (add-timer e (lambda () (setq cb :timeout)) 1.5)
      (event-dispatch e :timeout 2)
      (iseq cb :timeout))))
