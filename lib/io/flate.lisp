;;; io/flate.lisp --- Compressed IO Interface

;; Use compression (ZSTD) with Lisp objects and streams.

;;; Commentary:

;; compression ref: https://www.xach.com/lisp/salza2/ (compression only)

;; decompression ref: https://github.com/sharplispers/chipz (decompression only)

;; The libraries above are the current state-of-the-art for compression and
;; decompression in Common Lisp. They are portable packages which depend on
;; gray streams. They loosely cover deflate, zlib, gzip, and bzip2 data.

;; The compression backends are themselves hand-coded in Common Lisp, making
;; them excellent reference material. However, we don't have much use for the
;; compression backends offered.

;; We intend to almost exclusively support Zstd compression and decompression
;; using our ZSTD FFI Lisp system, so we'll make a new library - FLATE - which
;; provides a shared zstd compression/decompression to Lisp objects and
;; streams.

;;; Code:
(in-package :io/flate)
(zstd:load-zstd)

;;; Vars
(defparameter *compression-buffer-size* 4096)
(defparameter *decompression-buffer-size* 4096)
(defparameter *default-compression-level* (zstd:zstd-defaultclevel))

(defvar *compression-types* 
  (list :zstd :gzip :zlib :deflate)
  "List of available compression backend types available for use as the value of  *PREFERRED-COMPRESSION-TYPE*.")

(defvar *preferred-compression-type* :zstd
  "Preferred compression backend used by this Lisp system. Must be one of
*COMPRESSION-TYPES* and defaults to :ZSTD.")

(defvar *compression-level* *default-compression-level*)
(defvar *compressor* nil
  "The global COMPRESSOR object.")
(defvar *decompressor* nil
  "The global DECOMPRESSOR object.")
;;; Utils

;;; Conditions
(eval-always (deferror flate-error () () (:reporter t)))

(deferror compression-error (flate-error) () (:reporter t))
(deferror decompression-error (flate-error) () (:reporter t))

;;; Proto
(defgeneric decompress (output state input &key &allow-other-keys)
  (:documentation "Decompress INPUT using initial STATE, writing to OUTPUT. STATE is either a
DECOMPRESSION-STATE object for deflate-based compression or a
ZSTD-DECOMPRESSOR in the case of zstd."))

(defgeneric compress (input state &key &allow-other-keys)
  (:documentation "Compress INPUT using initial STATE, which may be a COMPRESSION-STATE object
for deflate-based compression or a ZSTD-COMPRESSOR in the case of zstd."))

(defgeneric finish-compression (self)
  (:documentation "Finish the data format and flush all pending
  data in the bitstream."))

(defgeneric finish-decompression (self)
  (:documentation "Flush all pending compressed input of decompressor SELF."))

;; TODO 2024-06-08: maybe move this to generic io/stream protocol - 'RESET'

(defgeneric compress-octet (octet compressor)
  (:documentation "Add OCTET to the compressed data of COMPRESSOR."))

(defgeneric compress-octet-vector (vector compressor &key start end)
  (:documentation "Add the octets of VECTOR to the compressed
  data of COMPRESSOR."))

(defgeneric make-compressing-stream (key stream &key &allow-other-keys)
  (:documentation "Return a new COMPRESSING-STREAM of kind KEY, optionally wrapping STREAM."))

(defgeneric make-decompressing-stream (key stream &key &allow-other-keys)
  (:documentation "Return a new DECOMPRESSING-STREAM of kind KEY, optionally wrapping STREAM."))

(defgeneric compress-object (obj))
(defgeneric decompress-object (obj))

(defgeneric compression-level (obj))
(defgeneric (setf compression-level) (new obj))

(defgeneric compress-with (self obj &key &allow-other-keys))
(defgeneric decompress-with (self obj &key &allow-other-keys))

(defgeneric compress-octet-vector (vector compressor &key start end &allow-other-keys))
(defgeneric decompress-octet-vector (vector decompressor &key start end &allow-other-keys))

;;; Compression
(defclass compressor () ((output :initarg :output :accessor output)))

(defmethod std:stream-of ((self compressor))
  (output self))

(defclass compressing-stream (fundamental-binary-output-stream) ())

(defmethod stream-element-type ((stream compressing-stream))
  '(unsigned-byte 8))

(defclass compressing-deflate-stream (compressing-stream)
  ((compressor :initarg :compressor :accessor compressor)))

(defmethod make-compressing-stream (compressor-class stream &rest args)
  (make-instance 'compressing-deflate-stream
    :compressor (apply 'make-instance compressor-class args)))

(defmethod stream-write-sequence ((self compressing-stream) seq &optional start end)
  (unless (open-stream-p self)
    (error 'stream-closed-error :stream self))
  (let ((vector (if (typep seq 'vector)
                    seq
                    (coerce seq 'vector))))
    (compress-octet-vector vector (compressor self) :start start :end end))
  seq)

;;; Decompression
(defclass decompressor () ((input :initarg :input :accessor input)))

(defmethod std:stream-of ((self decompressor))
  (input self))

(defclass decompressing-stream (fundamental-binary-input-stream) ())

(defmethod stream-element-type ((stream decompressing-stream))
  '(unsigned-byte 8))

(defclass decompressing-deflate-stream (wrapped-stream decompressing-stream)
  ((state :initarg :state :reader state)
   (kernel :initarg :kernel :reader kernel)
   (input-buffer :initform (make-array 4096 :element-type '(unsigned-byte 8))
                 :reader input-buffer)
   (input-position :initform 0 :accessor input-position)
   (input-size :initform 0 :accessor input-size)
   (output-buffer :initform (make-array 4096 :element-type '(unsigned-byte 8))
                  :reader output-buffer)
   (output-position :initform 0 :accessor output-position)
   (output-size :initform 0 :accessor output-size)))

(defmethod make-decompressing-stream (decompressor-class stream &rest args)
  (apply 'make-instance 'decompressing-deflate-stream args))

;;; Macros
(defmacro with-compressor ((var class
                                &rest initargs
                                &key &allow-other-keys)
                           &body body)
  `(let ((,var (make-instance ,class ,@initargs)))
     (multiple-value-prog1 
         (progn ,@body)
       (finish-compression ,var))))

(defmacro with-decompressor ((var class
                              &rest initargs
                              &key &allow-other-keys)
                             &body body)
  `(let ((,var (make-instance ,class ,@initargs)))
     (multiple-value-prog1
         (progn ,@body)
       (finish-decompression ,var))))

(defmacro with-compressing-stream ((stream output-stream &rest args
                                                         &key (level *compression-level*) 
                                                              (type *preferred-compression-type*))
                                   &body body)
  "Within BODY, STREAM is bound to a compressing stream for the given
compression LEVEL and OUTPUT-STREAM. The result of the last form of BODY is
returned."
  `(with-open-stream (,stream (apply 'make-compressing-stream ,type ,output-stream 
                                     :level ,level
                                     ',(std:remove-from-plist args :type :level)))
     ,@body))

(defmacro with-decompressing-stream ((stream input-stream &rest args &key (type *preferred-compression-type*)) &body body)
  "Within BODY, STREAM is bound to a decompressing stream for the given
INPUT-STREAM. The result of the last form of BODY is returned."
  `(with-open-stream (,stream (apply 'make-decompressing-stream 
                                     ,type ,input-stream 
                                     ',(std:remove-from-plist args :type)))
     ,@body))

;;; Compression Functions
(defun compress-stream (input output &key (level 3) (type *preferred-compression-type*))
  "Read the data from the INPUT octet stream, compress it, and write the result
to the OUTPUT octet stream."
  (with-compressing-stream (stream output :level level :type type)
    (let ((buffer (make-array *compression-buffer-size* :element-type '(unsigned-byte 8))))
      (do ((n (read-sequence buffer input) (read-sequence buffer input)))
          ((zerop n) t)
        (write-sequence buffer stream :end n)))))

(defun compress-file (input output &key (level 3) (type *preferred-compression-type*))
  "Read the data from the INPUT file, compress it, and write the result to the
OUTPUT file."
  (with-open-file (input-stream input :element-type '(unsigned-byte 8))
    (with-open-file (output-stream output :direction :output :element-type '(unsigned-byte 8))
      (compress-stream input-stream output-stream :level level :type type))))

(defun compress-buffer (buffer &key (start 0) end (level 3) (type *preferred-compression-type*))
  "Read the data between the START and END offsets in the BUFFER, compress it,
and return the resulting octet vector."
  (let ((end (or end (length buffer))))
    (io/stream:with-output-to-sequence (output)
      (with-compressing-stream (stream output :level level :type type)
        (write-sequence buffer stream :start start :end end)))))

;;; Decompression functions
(defun decompress-stream (input output &key (type *preferred-compression-type*))
  "Read the data from the INPUT octet stream, decompress it, and write the
result to the OUTPUT octet stream."
  (with-decompressing-stream (stream input :type type)
    (let ((buffer (make-array *compression-buffer-size* :element-type '(unsigned-byte 8))))
      (do ((n (read-sequence buffer stream) (read-sequence buffer stream)))
          ((zerop n) t)
        (write-sequence buffer output :end n)))))

(defun decompress-file (input output &key (type *preferred-compression-type*))
  "Read the data from the INPUT file, decompress it, and write the result to
the OUTPUT file."
  (with-open-file (input-stream input :element-type '(unsigned-byte 8))
    (with-open-file (output-stream output :direction :output :element-type '(unsigned-byte 8))
      (decompress-stream input-stream output-stream :type type))))

(defun decompress-buffer (buffer &key (start 0) end (type *preferred-compression-type*))
  "Read the data between the START and END offsets in the BUFFER, decompress
it, and return the resulting octet vector."
  (let ((end (or end (length buffer))))
    (io/stream:with-output-to-sequence (output)
      (io/stream:with-input-from-sequence (input buffer :start start :end end)
        (decompress-stream input output :type type)))))
