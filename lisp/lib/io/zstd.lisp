;;; zstd.lisp --- Zstd IO API

;; High-level Zstd API

;;; Code:
(in-package :io/zstd)

(eval-always (deferror zstd-error (io-error) () (:auto t)))

(deferror zstd-input-error (zstd-error) () (:auto t))
(deferror zstd-output-error (zstd-error) () (:auto t))

(deferror zstd-checksum-error (zstd-error) () (:auto t))
(deferror zstd-dictionary-error (zstd-error) () (:auto t))

(defclass zstd-compressing-stream (compressing-stream)
  ((%level :initform *compression-level* :accessor compression-level)
   (%input :initform (allocate-zstd-inbuffer) :reader input)
   (%output :initform (allocate-zstd-outbuffer) :reader output)
   (%stream :initform (zstd-createcstream)
            :type zstd-cstream
            :reader stream-of)))

(defmethod input-size ((self zstd-compressing-stream))
  (zstd-inbuffer-size (input self)))

(defmethod (setf input-size) (new (self zstd-compressing-stream))
  (setf (zstd-inbuffer-size (input self)) new))

(defmethod output-size ((self zstd-compressing-stream))
  (zstd-outbuffer-size (output self)))

(defmethod (setf output-size) (new (self zstd-compressing-stream))
  (setf (zstd-outbuffer-size (output self)) new))

(defmethod input-buffer ((self zstd-compressing-stream))
  (zstd-inbuffer-src (input self)))

(defmethod (setf input-buffer) ((new vector) (self zstd-compressing-stream))
  (setf (zstd-inbuffer-src (input self)) (octets-to-alien new)))

(defmethod (setf input-buffer) (new (self zstd-compressing-stream))
  (setf (zstd-inbuffer-src (input self)) new))

(defmethod output-buffer ((self zstd-compressing-stream))
  (zstd-outbuffer-dst (output self)))

(defmethod (setf output-buffer) (new (self zstd-compressing-stream))
  (setf (zstd-outbuffer-dst (output self)) new))

(defmethod (setf output-buffer) ((new vector) (self zstd-compressing-stream))
  (setf (zstd-outbuffer-dst (output self)) (octets-to-alien new)))

(defmethod input-position ((self zstd-compressing-stream))
  (zstd-inbuffer-pos (input self)))

(defmethod (setf input-position) (new (self zstd-compressing-stream))
  (setf (zstd-inbuffer-pos (input self)) new))

(defmethod output-position ((self zstd-compressing-stream))
  (zstd-outbuffer-pos (output self)))

(defmethod (setf output-position) (new (self zstd-compressing-stream))
  (setf (zstd-outbuffer-pos (output self)) new))

(defmethod initialize-instance :after ((self zstd-compressing-stream) 
                                       &key (input-size (zstd-cstreaminsize))
                                            (output-size (zstd-cstreamoutsize)))
  (setf (input-size self) input-size
        (output-size self) output-size)
  (zstd-initcstream (stream-of self) (compression-level self)))

(defmethod stream-force-output ((stream zstd-compressing-stream))
  (zstd::zstd-flushstream (stream-of stream) (output stream)))

(defmethod stream-finish-output ((stream zstd-compressing-stream))
  (zstd::zstd-endstream (stream-of stream) (output stream)))

(defmethod stream-write-sequence ((stream zstd-compressing-stream) (seq vector) &optional start end))
    
(defmethod close ((stream zstd-compressing-stream) &key &allow-other-keys)
  (sb-alien:free-alien (input stream))
  (sb-alien:free-alien (output stream))
  (zstd-freecstream (stream-of stream)))

(defclass zstd-decompressing-stream (decompressing-stream)
  ((%input :initform (allocate-zstd-inbuffer) :reader input)
   (%output :initform (allocate-zstd-outbuffer) :reader output)
   (%stream :initform (zstd-createdstream)
            :type zstd-dstream
            :reader stream-of)))

(defmethod input-size ((self zstd-decompressing-stream))
  (zstd-inbuffer-size (input self)))

(defmethod (setf input-size) (new (self zstd-decompressing-stream))
  (setf (zstd-inbuffer-size (input self)) new))

(defmethod output-size ((self zstd-decompressing-stream))
  (zstd-outbuffer-size (output self)))

(defmethod (setf output-size) (new (self zstd-decompressing-stream))
  (setf (zstd-outbuffer-size (output self)) new))

(defmethod input-buffer ((self zstd-decompressing-stream))
  (zstd-inbuffer-src (input self)))

(defmethod (setf input-buffer) (new (self zstd-decompressing-stream))
  (setf (zstd-inbuffer-src (input self)) new))

(defmethod output-buffer ((self zstd-decompressing-stream))
  (zstd-outbuffer-dst (output self)))

(defmethod (setf output-buffer) (new (self zstd-decompressing-stream))
  (setf (zstd-outbuffer-dst (output self)) new))

(defmethod input-position ((self zstd-decompressing-stream))
  (zstd-inbuffer-pos (input self)))

(defmethod (setf input-position) (new (self zstd-decompressing-stream))
  (setf (zstd-inbuffer-pos (input self)) new))

(defmethod output-position ((self zstd-decompressing-stream))
  (zstd-outbuffer-pos (output self)))

(defmethod (setf output-position) (new (self zstd-decompressing-stream))
  (setf (zstd-outbuffer-pos (output self)) new))

(defmacro with-zstd-stream (obj (zst in out) &body body)
  `(let ((stream (stream-of ,obj)))
    (let ((,zst (stream-of stream))
          (,in (input stream))
          (,out (output stream)))
      ,@body)))

(defmethod initialize-instance :after ((self zstd-decompressing-stream) 
                                       &key (input-size (zstd-dstreaminsize))
                                            (output-size (zstd-dstreamoutsize)))

  (zstd-initdstream (stream-of self))
  (setf (input-size self) input-size
        (output-size self) output-size))

(defmethod close ((stream zstd-decompressing-stream) &key &allow-other-keys)
  (sb-alien:free-alien (input stream))
  (sb-alien:free-alien (output stream))
  (zstd-freedstream (stream-of stream)))

(defclass zstd-compressor (compressor) ()
  (:default-initargs
   :stream (make-instance 'zstd-compressing-stream)))

(defmethod input ((self zstd-compressor))
  (input (stream-of self)))

(defmethod output ((self zstd-compressor))
  (output (stream-of self)))

(defmethod input-size ((self zstd-compressor))
  (input-size (stream-of self)))

(defmethod output-size ((self zstd-compressor))
  (output-size (stream-of self)))

(defmethod output-buffer ((self zstd-compressor))
  (output-buffer (stream-of self)))

(defmethod (setf output-buffer) (new (self zstd-compressor))
  (setf (output-buffer (stream-of self)) new))

(defmethod (setf output-buffer) ((new vector) (self zstd-compressor))
  (setf (zstd-outbuffer-dst (output self)) (octets-to-alien new)))

(defmethod input-buffer ((self zstd-compressor))
  (input-buffer (stream-of self)))

(defmethod (setf input-buffer) (new (self zstd-compressor))
  (setf (input-buffer (stream-of self)) new))

(defmethod (setf input-buffer) ((new vector) (self zstd-compressor))
  (setf (zstd-inbuffer-src (input self)) (octets-to-alien new)))

(defmethod input-position ((self zstd-compressor))
  (input-position (stream-of self)))

(defmethod (setf input-position) (new (self zstd-compressor))
  (setf (input-position (stream-of self)) new))

(defmethod output-position ((self zstd-compressor))
  (output-position (stream-of self)))

(defmethod (setf output-position) (new (self zstd-compressor))
  (setf (output-position (stream-of self)) new))

(defmethod compression-level ((self zstd-compressor))
  (compression-level (stream-of self)))

(defmethod compress-with ((self zstd-compressor) (obj vector) &key (end-op :continue) &allow-other-keys)
  (with-zstd-stream self (z i o)
    (setf 
     (zstd-inbuffer-src i)
     (octets-to-alien obj)
     (zstd-outbuffer-dst o)
     (make-alien sb-alien:unsigned-char (output-size self)))
    (zstd-compressstream2 z o i (zstd-enddirective end-op))))

(defmethod stream-force-output ((stream zstd-compressor))
  (stream-force-output (stream-of stream)))

(defmethod stream-finish-output ((stream zstd-compressor))
  (stream-finish-output (stream-of stream)))

(defclass zstd-decompressor (decompressor)
  ()
   (:default-initargs
    :stream (make-instance 'zstd-decompressing-stream)))

(defmethod input ((self zstd-decompressor))
  (input (stream-of self)))

(defmethod output ((self zstd-decompressor))
  (output (stream-of self)))

(defmethod input-buffer ((self zstd-decompressor))
  (input-buffer (stream-of self)))

(defmethod (setf input-buffer) (new (self zstd-decompressor))
  (setf (input-buffer (stream-of self)) new))

(defmethod (setf input-buffer) ((new vector) (self zstd-decompressor))
  (setf (zstd-inbuffer-src (input self)) (octets-to-alien new)))

(defmethod output-buffer ((self zstd-decompressor))
  (output-buffer (stream-of self)))

(defmethod (setf output-buffer) (new (self zstd-decompressor))
  (setf (output-buffer (stream-of self)) new))

(defmethod (setf output-buffer) ((new vector) (self zstd-decompressor))
  (setf (zstd-outbuffer-dst (output self)) (octets-to-alien new)))

(defmethod input-size ((self zstd-decompressor))
  (input-size (stream-of self)))

(defmethod output-size ((self zstd-decompressor))
  (output-size (stream-of self)))

(defmethod input-position ((self zstd-decompressor))
  (input-position (stream-of self)))

(defmethod (setf input-position) (new (self zstd-decompressor))
  (setf (input-position (stream-of self)) new))

(defmethod output-position ((self zstd-decompressor))
  (output-position (stream-of self)))

(defmethod (setf output-position) (new (self zstd-decompressor))
  (setf (output-position (stream-of self)) new))

(defmethod decompress-with ((self zstd-decompressor) (obj vector) &key &allow-other-keys)
  (with-zstd-stream self (z i o)
    (setf 
     (zstd-inbuffer-src i)
     (octets-to-alien obj)
     (zstd-outbuffer-dst o)
     (make-alien sb-alien:unsigned-char (input-size self)))
    (zstd-decompressstream z o i)))
;; (zstd::zstd-decompressbound

;;; Simple API
(defmacro with-zstd-output ((sym &optional buffer (level #.zstd:+zstd-clevel-default+)) &body body)
  `(handler-case
       (let ((,sym ,(or buffer
                        (make-array #.io/flate:*compression-buffer-size*
                                    :element-type 'std:octet
                                    :fill-pointer 0))))
         ,@body
         (zstd:zstdc ,sym ,level))
     (error (c) (zstd-output-error c))))

(defmacro with-zstd-input ((sym buffer &optional capacity) &body body)
  `(handler-case 
       (let ((,sym (zstd:zstdd ,buffer ,(or capacity `(length ,buffer)))))
         ,@(when (null body) `(,sym))
         ,@body)
     (error (c) (zstd-input-error c))))

(defmacro with-zstd-buffer (direction &body body)
  (ecase direction
    (:input `(with-zstd-input ,@body))
    (:output `(with-zstd-output ,@body))))
