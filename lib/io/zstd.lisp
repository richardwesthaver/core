;;; zstd.lisp --- Zstd IO API

;; High-level Zstd API

;;; Commentary:

;; AO <2026-04-12 Sun> this file has been rewritten based on CL-ZSTD.

;; ref: https://codeberg.org/glv/cl-zstd/src/branch/master/src/zstd.lisp

;;; Code:
(in-package :io/zstd)

;;; Conditions
(define-condition zstd-error (flate-error simple-error) ())

(defmacro zstd-error (message &rest args)
  `(error 'zstd-error
          :format-control ,message
          :format-arguments (list ,@args)))

(defmacro zstd-check (form)
  (with-gensyms (code)
    `(lety ((,code ,form :type (unsigned-byte 64)))
       (if (= (zstd:zstd-iserror ,code) 1)
           (zstd-error (zstd::zstd-geterrorname ,code))
           ,code))))

;;; Compression
(defclass zstd-compressing-stream (compressing-stream)
  ((output-stream :accessor output-stream)
   (state :accessor state)
   (input-buffer :accessor input-buffer)
   (zstd-in-buffer :accessor zstd-in-buffer)
   (output-buffer :accessor output-buffer)
   (zstd-out-buffer :accessor zstd-out-buffer)))

(defun compress-and-write (stream)
  (with-slots (output-stream state
               input-buffer zstd-in-buffer
               output-buffer zstd-out-buffer)
      stream
    (zstd-check (zstd-compressstream2 state
                                      zstd-out-buffer
                                      zstd-in-buffer
                                      :continue))
    (with-alien-slots (zstd::size zstd::pos) zstd-in-buffer
      (when (plusp zstd::pos)
        (replace input-buffer input-buffer :start2 zstd::pos :end2 zstd::size)
        (decf zstd::size zstd::pos)
        (setf zstd::pos 0)))
    (with-alien-slots (zstd::pos) zstd-out-buffer
      (when (plusp zstd::pos)
        (write-sequence output-buffer output-stream :end zstd::pos)
        (setf zstd::pos 0)))))

(defmethod stream-write-byte ((stream zstd-compressing-stream) byte)
  (with-slots (input-buffer zstd-in-buffer) stream
    (with-alien-slots (zstd::size zstd::pos) zstd-in-buffer
      (setf (aref input-buffer zstd::size) byte)
      (incf zstd::size)))
  (compress-and-write stream)
  byte)

(defmethod stream-write-sequence ((stream zstd-compressing-stream) seq &optional (start 0) (end (length seq)))
  (with-slots (input-buffer zstd-in-buffer) stream
    (loop while (< start end) 
          do (with-alien-slots (zstd::size) zstd-in-buffer
               (let* ((available-space (- (length input-buffer) zstd::size))
                      (n (min (- end start) available-space)))
                 (replace input-buffer seq
                          :start1 zstd::size :start2 start :end2 (+ start n))
                 (incf zstd::size n)
                 (incf start n)))
             (compress-and-write stream)))
  seq)

(defmethod stream-finish-output ((stream zstd-compressing-stream))
  (with-slots (output-stream state zstd-in-buffer
               output-buffer zstd-out-buffer)
      stream
    (do ((frame-complete-p nil))
        (frame-complete-p)
      (setf frame-complete-p
            (zerop (zstd-check (zstd-compressstream2 state
                                                     zstd-out-buffer
                                                     zstd-in-buffer
                                                     :end))))
      (when (plusp (slot zstd-out-buffer 'zstd::pos))
        (write-sequence output-buffer output-stream :end (slot zstd-out-buffer 'zstd::pos))
        (setf (slot zstd-out-buffer 'zstd::pos) 0)))

    (setf (slot zstd-in-buffer 'zstd::pos) 0
          (slot zstd-in-buffer 'zstd::size) 0)
    (setf (slot zstd-out-buffer 'zstd::pos) 0)
    ;; don't close the output-stream (FD-STREAM)
    ;; (finish-output output-stream)
    ))

(defmethod close ((stream zstd-compressing-stream) &key &allow-other-keys)
  (when (open-stream-p stream)
    (finish-output stream)
    (with-slots (state input-buffer zstd-in-buffer
                 output-buffer zstd-out-buffer)
        stream
      (zstd-check (zstd::zstd-freecctx state))
      ;; (setf state nil)
      ;; (setf input-buffer nil)
      ;; (free-alien zstd-in-buffer)
      ;; (setf zstd-in-buffer nil)
      ;; (setf output-buffer nil)
      ;; (free-alien zstd-out-buffer)
      ;; (setf zstd-out-buffer nil))
      ))
  t)

(defun initialize-context (context level)
  "Initialize the CONTEXT for the given compression LEVEL."
  (zstd-check (zstd::zstd-cctx-setparameter context :compression-level level))
  (zstd-check (zstd::zstd-cctx-setparameter context :checksum-flag 1))
  context)

(defmethod make-compressing-stream ((key (eql :zstd)) output-stream &key (level *compression-level*))
  "Return a stream that will compress the bytes written to it at the given
compression LEVEL and write them to the OUTPUT-STREAM."
  (let ((stream (make-instance 'zstd-compressing-stream))
        (input-buffer-size (zstd-cstreaminsize))
        (output-buffer-size (zstd-cstreamoutsize))
        (min-level (zstd::zstd-minclevel))
        (max-level (zstd::zstd-maxclevel)))
    (setf (output-stream stream) output-stream)
    (with-slots (zstd-context input-buffer zstd-in-buffer
                 output-buffer zstd-out-buffer)
        stream
      (if (and (integerp level) (<= min-level level max-level))
          (let ((context (zstd::zstd-createcctx)))
            (if (null-alien context)
                (zstd-error "Failed to create compression context.")
                (setf zstd-context (initialize-context context level))))
          (zstd-error "LEVEL must be between ~d and ~d." min-level max-level))
      (setf input-buffer (io/static:make-static-vector input-buffer-size :initial-element 0))
      (setf zstd-in-buffer (make-alien zstd-inbuffer))
      (let ((ffi-input-buffer (io/static:static-vector-pointer input-buffer)))
        (with-alien-slots (zstd::src zstd::size zstd::pos) zstd-in-buffer
          (setf zstd::src ffi-input-buffer)
          (setf zstd::size 0)
          (setf zstd::pos 0)))
      (setf output-buffer (io/static:make-static-vector output-buffer-size :initial-element 0))
      (setf zstd-out-buffer (make-alien zstd-outbuffer))
      (let ((ffi-output-buffer (io/static:static-vector-pointer output-buffer)))
        (with-alien-slots (zstd::dst zstd::size zstd::pos) (zstd-out-buffer stream)
          (setf zstd::dst ffi-output-buffer)
          (setf zstd::size output-buffer-size)
          (setf zstd::pos 0))))
    stream))

;;; Decompression
(defclass zstd-decompressing-stream (decompressing-stream)
  ((input-stream :accessor input-stream)
   ;; DCTX
   (state :accessor state)
   (input-buffer :accessor input-buffer)
   (zstd-in-buffer :accessor zstd-in-buffer)
   (output-buffer :accessor output-buffer)
   (zstd-out-buffer :accessor zstd-out-buffer)
   (frame-complete-p :accessor frame-complete-p)))

(defun read-and-decompress (stream)
  (with-slots (input-stream state input-buffer zstd-in-buffer
               zstd-out-buffer frame-complete-p)
      stream
    (let ((end-of-input-p nil))
      (with-alien-slots (zstd::size) zstd-in-buffer
        (setf zstd::size (read-sequence input-buffer input-stream :start zstd::size))
        (setf end-of-input-p (zerop zstd::size)))
      (unless end-of-input-p
        (setf frame-complete-p
              (zerop (zstd-check (zstd-decompressstream 
                                  state
                                  zstd-out-buffer
                                  zstd-in-buffer))))
        (with-alien-slots (zstd::size zstd::pos) zstd-in-buffer
          (when (plusp zstd::pos)
            (replace input-buffer input-buffer :start2 zstd::pos :end2 zstd::size)
            (decf zstd::size zstd::pos)
            (setf zstd::pos 0))))
      end-of-input-p)))

(defmethod stream-listen ((stream zstd-decompressing-stream))
  (with-slots (input-stream zstd-in-buffer zstd-out-buffer) stream
    (or (plusp (slot zstd-out-buffer 'zstd::pos))
        (plusp (slot zstd-in-buffer 'zstd::size))
        (listen input-stream))))

(defmethod stream-read-byte ((stream zstd-decompressing-stream))
  (with-slots (output-buffer zstd-out-buffer) stream
    (let ((end-of-input-p (read-and-decompress stream)))
      (with-alien-slots (zstd::pos) zstd-out-buffer
        (cond
          ((plusp zstd::pos)
           (let ((byte (aref output-buffer 0)))
             (replace output-buffer output-buffer :start2 1 :end2 zstd::pos)
             (decf zstd::pos)
             byte))
          ((and end-of-input-p (not (frame-complete-p stream)))
           (zstd-error "Truncated stream."))
          (t
           :eof))))))

(defmethod stream-read-sequence ((stream zstd-decompressing-stream) seq &optional (start 0) end)
  (with-slots (output-buffer zstd-out-buffer) stream
    (let ((end-of-input-p nil)
          (end (or end (length seq))))
      (loop until (or (= start end) end-of-input-p) :do
        (setf end-of-input-p (read-and-decompress stream))
        (with-alien-slots (zstd::pos) zstd-out-buffer
          (loop :while (and (< start end) (plusp zstd::pos)) :do
            (let ((n (min (- end start) zstd::pos)))
              (replace seq output-buffer :start1 start :end2 n)
              (replace output-buffer output-buffer :start2 n :end2 zstd::pos)
              (decf zstd::pos n)
              (incf start n)))))
      (when (and end-of-input-p (not (frame-complete-p stream)))
        (zstd-error "Truncated stream."))))
  start)

(defmethod close ((stream zstd-decompressing-stream) &key &allow-other-keys)
  (when (open-stream-p stream)
    (with-slots (state input-buffer zstd-in-buffer
                 output-buffer zstd-out-buffer)
        stream
      (zstd-check (zstd::zstd-freedctx state))
      (setf state nil)
      (setf input-buffer nil)
      (free-alien zstd-in-buffer)
      (setf zstd-in-buffer nil)
      (setf output-buffer nil)
      (free-alien zstd-out-buffer)
      (setf zstd-out-buffer nil)))
  t)

(defmethod make-decompressing-stream ((key (eql :zstd)) input-stream &key)
  "Return a stream that will supply the bytes resulting from the decompression
of the data read from the INPUT-STREAM."
  (let ((stream (make-instance 'zstd-decompressing-stream))
        (input-buffer-size (zstd::zstd-dstreaminsize))
        (output-buffer-size (zstd::zstd-dstreamoutsize)))
    (setf (input-stream stream) input-stream)
    (with-slots (state input-buffer zstd-in-buffer
                 output-buffer zstd-out-buffer frame-complete-p)
        stream
      (let ((context (zstd::zstd-createdctx)))
        (if (null-alien context)
            (zstd-error "Failed to create decompression context.")
            (setf state context)))
      (setf input-buffer (io/static:make-static-vector input-buffer-size :initial-element 0))
      (setf zstd-in-buffer (make-alien zstd-inbuffer))
      (let ((ffi-input-buffer (io/static:static-vector-pointer input-buffer)))
        (with-alien-slots (zstd::src zstd::size zstd::pos) zstd-in-buffer
          (setf zstd::src ffi-input-buffer)
          (setf zstd::size 0)
          (setf zstd::pos 0)))
      (setf output-buffer (io/static:make-static-vector output-buffer-size :initial-element 0))
      (setf zstd-out-buffer (make-alien zstd-outbuffer))
      (let ((ffi-output-buffer (io/static:static-vector-pointer output-buffer)))
        (with-alien-slots (zstd::dst zstd::size zstd::pos) zstd-out-buffer
          (setf zstd::dst ffi-output-buffer)
          (setf zstd::size output-buffer-size)
          (setf zstd::pos 0)))
      (setf frame-complete-p t))
    stream))
