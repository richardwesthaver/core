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
   (%input :initform (allocate-zstd-inbuffer) :reader %input-of)
   (%input-size :initform (zstd-cstreaminsize) :reader %input-size-of)
   (%output :initform (allocate-zstd-outbuffer) :reader %output-of)
   (%output-size :initform (zstd-cstreamoutsize) :reader %output-size-of)
   (%stream :initform (zstd-createcstream)
            :type zstd-cstream
            :reader %stream-of)))

(defmethod initialize-instance :after ((self zstd-compressing-stream) &key &allow-other-keys)
  (zstd-initcstream (%stream-of self) (compression-level self))
  (setf (zstd-inbuffer-size (%input-of self))
        (%input-size-of self)
        (zstd-outbuffer-size (%output-of self))
        (%output-size-of self)))

(defmethod stream-force-output ((stream zstd-compressing-stream))
  (zstd-compressstream2 (%stream-of stream) 
                             (%output-of stream)
                             (%input-of stream)
                             (zstd-enddirective :flush)))

(defmethod stream-finish-output ((stream zstd-compressing-stream))
  (zstd-compressstream2 (%stream-of stream)
                             (%output-of stream)
                             (%input-of stream)
                             (zstd-enddirective :end)))

(defmethod close :after ((stream zstd-compressing-stream) &key &allow-other-keys)
  (zstd-freecstream (%stream-of stream))
  (sb-alien:free-alien (%input-of stream))
  (sb-alien:free-alien (%output-of stream)))

(defclass zstd-decompressing-stream (decompressing-stream)
  ((%input :initform (allocate-zstd-inbuffer) :reader %input-of)
   (%input-size :initform (zstd-dstreaminsize) :reader %input-size-of)
   (%output :initform (allocate-zstd-outbuffer) :reader %output-of)
   (%output-size :initform (zstd-dstreamoutsize) :reader %output-size-of)
   (%stream :initform (zstd-createdstream)
            :type zstd-dstream
            :reader %stream-of)))

(defmacro with-zstd-stream (obj (zst in out) &body body)
  `(let ((stream (stream-of ,obj)))
    (let ((,zst (%stream-of stream))
          (,in (%input-of stream))
          (,out (%output-of stream)))
      ,@body)))

(defmethod initialize-instance :after ((self zstd-decompressing-stream) &key &allow-other-keys)
  (zstd-initdstream (%stream-of self))
  (setf (zstd-inbuffer-size (%input-of self))
        (%input-size-of self)
        (zstd-outbuffer-size (%output-of self))
        (%output-size-of self)))

(defmethod close :after ((stream zstd-decompressing-stream) &key &allow-other-keys)
  (zstd-freedstream (%stream-of stream))
  (sb-alien:free-alien (%input-of stream))
  (sb-alien:free-alien (%output-of stream)))

(defclass zstd-compressor (compressor) ()
  (:default-initargs
   :stream (make-instance 'zstd-compressing-stream)))

(defmethod compression-level ((self zstd-compressor))
  (compression-level (stream-of self)))

(defclass zstd-decompressor (decompressor)
  ()
   (:default-initargs
    :stream (make-instance 'zstd-decompressing-stream)))

(defmethod decompress-with ((self zstd-decompressor) (obj vector) &key &allow-other-keys)
  (with-zstd-stream self (z i o)
    (setf 
     (zstd-inbuffer-src i)
     (octets-to-alien obj))
    (zstd-decompressstream z o i)))

(defmethod compress-with ((self zstd-compressor) (obj vector) &key (end-op :continue) &allow-other-keys)
  (with-zstd-stream self (z i o)
    (setf 
     (zstd-inbuffer-src i)
     (octets-to-alien obj))
    (zstd-compressstream2 z o i (zstd-enddirective end-op))))

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
