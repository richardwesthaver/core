;;; zstd.lisp --- Zstd IO API

;; High-level Zstd API

;;; Code:
(in-package :io/zstd)

;;; Conditions
(eval-always (deferror zstd-error (io-error flate-error) () (:auto t)))

(deferror zstd-input-error (zstd-error) () (:auto t))
(deferror zstd-output-error (zstd-error) () (:auto t))

(deferror zstd-checksum-error (zstd-error) () (:auto t))
(deferror zstd-dictionary-error (zstd-error) () (:auto t))

;;; Objects
(defclass zstd-compressing-stream (compressing-stream)
  ((%level :initform *compression-level* :accessor compression-level)
   (%input :initform (allocate-zstd-inbuffer) :reader input)
   (%output :initform (allocate-zstd-outbuffer) :reader output)
   (%stream :initform (zstd-createcstream)
            :type (alien (* zstd-cstream))
            :reader cstream)))

(defmethod make-compressing-stream ((self (eql :zstd)) 
                                    &optional (stream 
                                               (make-instance 'sb-gray:fundamental-binary-input-stream)))
                                                   
  (make-instance 'zstd-compressing-stream :stream stream))

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
  (zstd-initcstream (cstream self) (compression-level self)))

(defmethod stream-force-output ((stream zstd-compressing-stream))
  (zstd::zstd-flushstream (cstream stream) (output stream)))

(defmethod stream-finish-output ((stream zstd-compressing-stream))
  (zstd::zstd-endstream (cstream stream) (output stream)))

(defmethod stream-write-sequence ((stream zstd-compressing-stream) (seq vector) &optional start end))
    
(defmethod close ((stream zstd-compressing-stream) &key &allow-other-keys)
  ;; (sb-alien:free-alien (input stream))
  ;; (sb-alien:free-alien (output stream))
  ;; (zstd-freecstream (cstream stream))
  )

(defclass zstd-decompressing-stream (decompressing-stream)
  ((%input :initform (allocate-zstd-inbuffer) :reader input :type (alien zstd-inbuffer))
   (%output :initform (allocate-zstd-outbuffer) :reader output :type (alien zstd-outbuffer))
   (%stream :initform (zstd-createdstream)
            :type (alien (* zstd-dstream))
            :reader dstream)))

(defmethod make-decompressing-stream ((self (eql :zstd)) 
                                      &optional (stream 
                                               (make-instance 'sb-gray:fundamental-binary-output-stream)))
  (make-instance 'zstd-decompressing-stream :stream stream))

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

(defmacro with-zstd-stream (stream (zst in out) &body body)
  `(let ((,zst (slot-value ,stream '%stream))
         (,in (input ,stream))
         (,out (output ,stream)))
     ,@body))

(defmethod initialize-instance :after ((self zstd-decompressing-stream)
                                       &key (input-size (zstd-dstreaminsize))
                                            (output-size (zstd-dstreamoutsize)))
  (setf (input-size self) input-size
        (output-size self) output-size)
  ;; returns recommended
  (print (zstd-initdstream (dstream self))))

(defmethod close ((stream zstd-decompressing-stream) &key &allow-other-keys)
  ;; (sb-alien:free-alien (input stream))
  ;; (sb-alien:free-alien (output stream))
  (zstd-freedstream (dstream stream)))

(defmethod sb-gray:stream-read-sequence ((self zstd-decompressing-stream) (seq vector) &optional start end)
  (declare (ignore start end))
  (with-vector-sap (sp seq)
    (with-zstd-stream self (z i o)
      (setf
       (zstd-outbuffer-dst o) sp
       (zstd-outbuffer-size o) (output-size self))
      (zstd-decompressstream z o i))))

(defclass zstd-compressor (compressor) ()
  (:default-initargs
   :stream (make-instance 'zstd-compressing-stream)))

(defmethod cstream ((self zstd-compressor))
  (cstream (stream-of self)))

(defmethod input ((self zstd-compressor))
  (input (stream-of self)))

(defmethod output ((self zstd-compressor))
  (output (stream-of self)))

(defmethod input-size ((self zstd-compressor))
  (input-size (stream-of self)))

(defmethod output-size ((self zstd-compressor))
  (output-size (stream-of self)))

(defmethod (setf output-size) (new (self zstd-compressor))
  (setf (output-size (stream-of self)) new))

(defmethod output-buffer ((self zstd-compressor))
  (output-buffer (stream-of self)))

(defmethod (setf output-buffer) (new (self zstd-compressor))
  (setf (output-buffer (stream-of self)) new))

(defmethod (setf output-buffer) ((new vector) (self zstd-compressor))
  (memcpy (zstd-outbuffer-dst (output self)) (octets-to-alien new) (length new)))

(defmethod input-buffer ((self zstd-compressor))
  (input-buffer (stream-of self)))

(defmethod (setf input-buffer) (new (self zstd-compressor))
  (setf (input-buffer (stream-of self)) new))

(defmethod (setf input-buffer) ((new vector) (self zstd-compressor))
  (memcpy (zstd-inbuffer-src (input self)) (octets-to-alien new) (length new)))

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
  (with-zstd-stream (stream-of self) (z i o)
    (setf 
     (zstd-inbuffer-src i)
     (octets-to-alien obj)
     (zstd-outbuffer-dst o)
     (make-alien sb-alien:unsigned-char (output-size self)))
    (let ((code (zstd-compressstream2 z o i (zstd-enddirective end-op))))
      (if (or (zerop code) (zerop (zstd::zstd-iserror code)))
          code
          (zstd-error (zstd::zstd-geterrorstring (zstd::zstd-geterrorcode code)))))))

(defmethod stream-force-output ((stream zstd-compressor))
  (force-output (stream-of stream)))

(defmethod stream-finish-output ((stream zstd-compressor))
  (stream-finish-output (stream-of stream)))

(defclass zstd-decompressor (decompressor)
  ()
   (:default-initargs
    :stream (make-instance 'zstd-decompressing-stream)))

(defmethod dstream ((self zstd-decompressor))
  (dstream (stream-of self)))

(defmethod input ((self zstd-decompressor))
  (input (stream-of self)))

(defmethod output ((self zstd-decompressor))
  (output (stream-of self)))

(defmethod input-buffer ((self zstd-decompressor))
  (input-buffer (stream-of self)))

(defmethod (setf input-buffer) (new (self zstd-decompressor))
  (setf (input-buffer (stream-of self)) new))

(defmethod (setf input-buffer) ((new vector) (self zstd-decompressor))
  (memcpy (zstd-inbuffer-src (input self)) (octets-to-alien new) (length new)))

(defmethod output-buffer ((self zstd-decompressor))
  (output-buffer (stream-of self)))

(defmethod (setf output-buffer) (new (self zstd-decompressor))
  (setf (output-buffer (stream-of self)) new))

(defmethod (setf output-buffer) ((new vector) (self zstd-decompressor))
  (memcpy (zstd-outbuffer-dst (output self)) (octets-to-alien new) (length new)))

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
  (with-zstd-stream (stream-of self) (z i o)
    (setf 
     (zstd-inbuffer-src i)
     (octets-to-alien obj)
     (zstd-outbuffer-dst o)
     (make-alien sb-alien:unsigned-char (input-size self)))
    (let ((code (zstd-decompressstream z o i)))
      (if (or (zerop code) (zerop (zstd::zstd-iserror code)))
          code
          (zstd-error (zstd::zstd-geterrorstring (zstd::zstd-geterrorcode code)))))))

;; (defmethod stream-force-output ((stream zstd-decompressor))
;;   (force-output (stream-of stream)))

(defmethod stream-finish-output ((stream zstd-decompressor))
  (stream-finish-output (stream-of stream)))

;; (defmethod stream-force-output ((stream zstd-decompressing-stream))
;;   (zstd::zstd-flushstream (dstream stream) (output stream)))

(defmethod stream-finish-output ((stream zstd-compressing-stream))
  (zstd::zstd-freedstream (dstream stream)))
       
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

(defmacro with-zstd-input ((sym buffer &optional size) &body body)
  `(handler-case 
       (let ((,sym (zstd:zstdd ,buffer ,(or size `(length ,buffer)))))
         ,@(when (null body) `(,sym))
         ,@body)
     (error (c) (zstd-input-error c))))

(defmacro with-zstd-buffer ((sym buffer &key size (level #.zstd:+zstd-clevel-default+) (direction :input)) &body body)
  (ecase direction
    (:input `(with-zstd-input (,sym ,buffer ,size) ,@body))
    (:output `(with-zstd-output (,sym ,buffer ,level) ,@body))))
