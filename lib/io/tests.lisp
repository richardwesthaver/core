(defpackage :io/tests
  (:use :cl :std :rt :io :uring :zstd :sb-gray :disk :disk/btrfs :io/stream :io/deflate :kbd
    :io/mux :io/sys))

(in-package :io/tests)
(defsuite :io)
(in-suite :io)

(load-uring)
(load-zstd)
(btrfs:load-btrfs)
(deftest sanity () (is (plusp (uring::io-uring-major-version))))

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
    (isequal "fo" (concatenate 'string (peeked (make-instance 'peeking-input-stream :stream s :count 2 :element-type 'character))))))

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
    (istype 'block-input-stream (make-instance 'block-input-stream))))

(deftest zstd-stream ()
  (let ((f (tmp-path "foo"))
        (f1 (tmp-path "foo")))
    (is (compress-file (system-relative-pathname :io "tests.lisp") f :type :zstd))
    (is (decompress-file f f1 :type :zstd))
    (delete-file f)
    (delete-file f1)))

;;; Deflate
(defparameter *data-size* (ash 1024 4))

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

(deftest bzip2 (:skip :todo))

(deftest zlib (:skip :todo))

(deftest lzw (:skip :todo))

;;; Static Vectors
(deftest static-vector ()
  (with-static-vector (v 4 :initial-element 0)
    (isequalp #(0 0 0 0) v))
  (isequalp #(1 2 3 4) (make-static-vector 4 :initial-contents '(1 2 3 4)))
  (with-static-stream (s)
    (dotimes (i 20)
      (is= (1+ (write-byte i s))
           (offset s)))
    (let ((k (symbol-name (gensym "static-stream-test"))))
      (write-sequence (string-to-octets k) s)
      ;; todo: (read-sequence)
      (isequalp (octets-to-string 
                 (buffer s) 
                 :start (offset s) 
                 :end (+ (offset s) (length k))) 
                k))
    (buffer s)))

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
  (islist (btrfs-subvolumes "/")))

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

(deftest sys ()
  (iseql 'minusp (io/sys::syscall-error-predicate #.(parse-alien-type 'int nil))))

(deftest mux-timer-delay ()
  (with-event-base (e)
    (let ((cb nil))
      (add-timer e (lambda () (setq cb :timeout)) 1.5)
      (event-dispatch e :timeout 2.0)
      (iseq cb :timeout))))

(deftest mux-timer-no-delay ()
  (with-event-base (e)
    (let ((cb nil))
      (add-timer e (lambda () (setq cb :timeout)) 0)
      (event-dispatch e :oneshot t)
      (iseq cb :timeout))))

(deftest mux-timeout-no-loop ()
  (with-event-base (e)
    (let ((cb nil))
      (add-timer e (lambda () (setq cb :timeout)) 1.5)
      (event-dispatch e :timeout 2)
      (iseq cb :timeout))))
