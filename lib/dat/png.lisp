;;; dat/png.lisp --- PNG image format

;; based on ZPNG by Zachary Beane

;;; Code:
(in-package :dat/png)

;;; ZPNG
;;;; Copyright (c) 2007 Zachary Beane, All Rights Reserved

;;;; Specials
(defvar *color-types*
  '((:grayscale . 0)
    (:truecolor . 2)
    (:indexed-color . 3)
    (:grayscale-alpha . 4)
    (:truecolor-alpha . 6)))

(defvar *png-signature*
  (make-array 8
              :element-type '(unsigned-byte 8)
              :initial-contents '(137 80 78 71 13 10 26 10)))

(defconstant +png-compression-method+ 0)
(defconstant +png-filtering+ 0)
(defconstant +png-interlace+ 0)

;;;; Utils
(defun write-uint32 (integer stream)
  (write-byte (ldb (byte 8 24) integer) stream)
  (write-byte (ldb (byte 8 16) integer) stream)
  (write-byte (ldb (byte 8  8) integer) stream)
  (write-byte (ldb (byte 8  0) integer) stream))

(defun checksum (data count)
  (let ((checksum (ironclad:make-digest :crc32)))
    (ironclad:update-digest checksum data :start 0 :end count)
    (octets-to-integer (ironclad:produce-digest checksum))))

(defclass chunk ()
  ((buffer :initarg :buffer :reader buffer)
   (pos :initform 4 :accessor pos)))

(defun chunk-write-byte (byte chunk)
  "Save one byte to CHUNK."
  (setf (aref (buffer chunk) (pos chunk)) byte)
  (incf (pos chunk)))

(defun chunk-write-uint32 (integer chunk)
  "Save INTEGER to CHUNK as four bytes."
  (let ((buffer (buffer chunk))
        (i (pos chunk)))
    (setf (aref buffer (+ i 0)) (ldb (byte 8 24)  integer)
          (aref buffer (+ i 1)) (ldb (byte 8 16)  integer)
          (aref buffer (+ i 2)) (ldb (byte 8  8)  integer)
          (aref buffer (+ i 3)) (ldb (byte 8  0)  integer)
          (pos chunk) (+ i 4))))

(defun make-chunk (a b c d size)
  "Make a chunk that uses A, B, C, and D as the signature bytes, with
data size SIZE."
  (let ((buffer (make-array (+ size 4) :element-type '(unsigned-byte 8))))
    (setf (aref buffer 0) a
          (aref buffer 1) b
          (aref buffer 2) c
          (aref buffer 3) d)
    (make-instance 'chunk
      :buffer buffer)))

(defun write-chunk (chunk stream)
  (write-uint32 (- (pos chunk) 4) stream)
  (write-sequence (buffer chunk) stream :end (pos chunk))
  (write-uint32 (checksum (buffer chunk) (pos chunk)) stream))

;;;; Conditions
(define-condition png-error (dat-error) ())

(define-condition invalid-size (png-error)
  ((width
    :initarg :width
    :reader invalid-size-width)
   (height
    :initarg :height
    :reader invalid-size-height))
  (:report (lambda (condition stream)
             (format stream "Invalid PNG size ~Ax~A; ~
                             both width and height must be positive"
                     (invalid-size-width condition)
                     (invalid-size-height condition)))))

(define-condition invalid-row-length (png-error)
  ((expected-length
    :initarg :expected-length
    :accessor invalid-row-length-expected-length)
   (actual-length
    :initarg :actual-length
    :accessor invalid-row-length-actual-length))
  (:report (lambda (condition stream)
             (format stream "Invalid row length ~A (expected ~A)"
                     (invalid-row-length-actual-length condition)
                     (invalid-row-length-expected-length condition)))))

(define-condition insufficient-rows (png-error)
  ((written
    :initarg :written
    :accessor insufficient-rows-written)
   (needed
    :initarg :needed
    :accessor insufficient-rows-needed))
  (:report (lambda (condition stream)
             (format stream "Insufficient rows written; need ~A, but only ~A ~
                             written"
                     (insufficient-rows-needed condition)
                     (insufficient-rows-written condition)))))

(define-condition incomplete-row (png-error)
  ((written
    :initarg :written
    :accessor incomplete-row-written)
   (needed
    :initarg :needed
    :accessor incomplete-row-needed))
  (:report (lambda (condition stream)
             (format stream "Incomplete row started; need ~A, but only ~A ~
                             written"
                     (incomplete-row-needed condition)
                     (incomplete-row-written condition)))))

(define-condition too-many-rows (png-error)
  ((count
    :initarg :count
    :accessor too-many-rows-count))
  (:report (lambda (condition stream)
             (format stream "Too many rows written for PNG; maximum row count ~
                             is ~A"
                     (too-many-rows-count condition)))))

(define-condition color-type-mismatch (png-error)
  ((given
    :initarg :given
    :accessor color-type-mismatch-given)
   (expected
    :initarg :expected
    :accessor color-type-mismatch-expected))
  (:report (lambda (condition stream)
	     (format stream "Wrong number of samples for PNG pixel; need ~A, ~
                             but only ~A written"
		     (color-type-mismatch-expected condition)
		     (color-type-mismatch-given condition)))))

;;;; PNG
(defclass base-png ()
  ((width :initarg :width :reader width)
   (height :initarg :height :reader height)
   (color-type :initform :truecolor :initarg :color-type :reader color-type)
   (bpp :initform 8 :initarg :bpp :reader bpp)))

(defclass png (base-png)
  ((image-data :initarg :image-data :reader image-data
               :writer (setf %image-data))
   (data-array :reader data-array
               :writer (setf %data-array))))

(defclass streamed-png (base-png)
  ((rows-written
    :initarg :rows-written
    :accessor rows-written)
   (row-data
    :initarg :row-data
    :accessor row-data)
   (compressor
    :initarg :compressor
    :accessor compressor)
   (output-stream
    :initarg :output-stream
    :accessor output-stream))
  (:default-initargs
   :rows-written 0))

(defclass pixel-streamed-png (streamed-png)
  ((current-offset
    :initform 0
    :accessor current-offset)))

(defgeneric ihdr-color-type (png))
(defgeneric samples-per-pixel (png))
(defgeneric scanline-offset (png scanline))
(defgeneric rowstride (png))

(defgeneric write-png-header (png stream))
(defgeneric write-ihdr (png stream))
(defgeneric write-idat (png stream))
(defgeneric write-iend (png stream))

(defgeneric copy-png (png))
(defgeneric png= (png1 png2))

(defgeneric write-png-stream (png stream))
(defgeneric write-png (png pathname &key if-exists))

(defgeneric start-png (png stream))
(defgeneric write-row (row png &key start end))
(defgeneric finish-png (png))
(defgeneric rows-left (png))

(defgeneric write-pixel (pixel png))
(defgeneric pixels-left-in-row (png))

(defmethod slot-unbound (class (png png) (slot (eql 'data-array)))
  (let ((array (make-array (list (height png)
                                 (width png)
                                 (samples-per-pixel png))
                           :displaced-to (image-data png)
                           :element-type '(unsigned-byte 8))))
    (setf (%data-array png) array)))

(defun check-size (png)
  (let ((width (width png))
        (height (height png)))
    (unless (and (plusp width) (plusp height))
      (error 'invalid-size
             :width width
             :height height))))

(defmethod initialize-instance :after ((png png) &rest args &key image-data)
  (declare (ignore args))
  (check-size png)
  (unless (or image-data (slot-boundp png 'image-data))
    (setf (%image-data png)
          (make-array (* (height png) (rowstride png))
                      :initial-element 0
                      :element-type '(unsigned-byte 8)))))

(defmethod ihdr-color-type (png)
  (cdr (assoc (color-type png) *color-types*)))

(defmethod samples-per-pixel (png)
  (ecase (color-type png)
    (:grayscale 1)
    (:truecolor 3)
    (:indexed-color 1)
    (:grayscale-alpha 2)
    (:truecolor-alpha 4)))

(defmethod rowstride (png)
  (* (width png) (samples-per-pixel png)))

(defmethod scanline-offset (png scanline)
  (* scanline (rowstride png)))


(defmethod write-png-header (png stream)
  (write-sequence *png-signature* stream))

(defmethod write-ihdr (png stream)
  (let ((chunk (make-chunk 73 72 68 82 13)))
    (chunk-write-uint32 (width png) chunk)
    (chunk-write-uint32 (height png) chunk)
    (chunk-write-byte (bpp png) chunk)
    (chunk-write-byte (ihdr-color-type png) chunk)
    (chunk-write-byte +png-compression-method+ chunk)
    (chunk-write-byte +png-filtering+ chunk)
    (chunk-write-byte +png-interlace+ chunk)
    (write-chunk chunk stream)))

(defun make-idat-callback (stream)
  (let* ((idat (make-chunk 73 68 65 84 16384))
         (buffer (buffer idat)))
    (lambda (data end)
      (replace buffer data :start1 4 :end2 end)
      (setf (pos idat) (+ end 4))
      (write-chunk idat stream))))

(defmethod write-idat (png stream)
  (let ((callback (make-idat-callback stream)))
    ;; TODO 2025-06-11: 
    (io/flate:with-compressor (compressor 'io/deflate:zlib-compressor
                                          :callback callback)
      (dotimes (i (height png))
        (let* ((start-offset (scanline-offset png i))
               (end-offset (+ start-offset (rowstride png))))
          (io/flate:compress-octet 0 compressor)
          (io/flate:compress-octet-vector (image-data png)
                                          compressor
                                          :start start-offset
                                          :end end-offset))))))

(defmethod write-iend (png stream)
  (let ((chunk (make-chunk 73 69 78 68 0)))
    (write-chunk chunk stream)))

(defmethod write-png-stream (png stream)
  (check-size png)
  (write-png-header png stream)
  (write-ihdr png stream)
  (write-idat png stream)
  (write-iend png stream))

(defmethod write-png (png file &key (if-exists :supersede))
  (check-size png)
  (with-open-file (stream file
                          :direction :output
                          :if-exists if-exists
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8))
    (write-png-stream png stream)
    (truename file)))

(defmethod copy-png (orig)
  (make-instance 'png
    :width (width orig)
    :height (height orig)
    :color-type (color-type orig)
    :bpp (bpp orig)
    :image-data (copy-seq (image-data orig))))

(defmethod png= (png1 png2)
  (or (eq png1 png2)
      (and (= (width png1)  (width png2))
           (= (height png1) (height png2))
           (= (bpp png1) (bpp png2))
           (eq (color-type png1) (color-type png2))
           (let ((png1.data (image-data png1))
                 (png2.data (image-data png2)))
             (not (mismatch png1.data png2.data))))))

;;; Streamed PNG methods
(defmethod slot-unbound (class (png streamed-png) (slot (eql 'row-data)))
  (let ((data (make-array (rowstride png) :element-type '(unsigned-byte 8)
                                          :initial-element 0)))
    (setf (row-data png) data)))

(defmethod start-png ((png streamed-png) stream)
  (setf (output-stream png) stream)
  (write-png-header png stream)
  (write-ihdr png stream)
  (setf (compressor png)
        (make-instance 'io/deflate:zlib-compressor
          :callback (make-idat-callback stream)))
  stream)

(defmethod start-png ((png pixel-streamed-png) stream)
  (setf (current-offset png) 0)
  (call-next-method))

(defmethod write-row (row (png streamed-png) &key (start 0) end)
  (let ((rowstride (rowstride png)))
    (setf end (or end (+ start rowstride)))
    (let ((row-length (- end start)))
      (unless (= (- end start) (rowstride png))
        (error 'invalid-row-length
               :expected-length rowstride
               :actual-length row-length))
      (unless (< (rows-written png) (height png))
        (error 'too-many-rows :count (height png)))
      (let ((compressor (compressor png)))
        (io/flate:compress-octet 0 compressor)
        (io/flate:compress-octet-vector row compressor :start start :end end)
        (incf (rows-written png))))))

(defmethod reset ((png streamed-png) &key)
  (setf (rows-written png) 0)
  (slot-makunbound png 'compressor)
  (slot-makunbound png 'output-stream)
  (fill (row-data png) 0))

(defmethod reset ((png pixel-streamed-png) &key)
  (setf (current-offset png) 0)
  (call-next-method))

(defmethod finish-png ((png streamed-png))
  (when (/= (rows-written png) (height png))
    (error 'insufficient-rows
           :written (rows-written png)
           :needed (height png)))
  (io/flate:finish-compression (compressor png))
  (write-iend png (output-stream png))
  (reset png)
  png)

(defmethod finish-png ((png pixel-streamed-png))
  (let* ((color-channels (samples-per-pixel png))
	 (columns (/ (current-offset png) color-channels)))
    (unless (zerop columns)
      (error 'incomplete-row
	     :written columns
	     :needed (/ (length (row-data png)) color-channels))))
  (call-next-method))

(defmethod rows-left ((png streamed-png))
  (- (height png) (rows-written png)))

(defmethod write-pixel (pixel (png pixel-streamed-png))
  (let ((row-data (row-data png))
	(samples-per-pixel (length pixel))
	(samples-per-pixel-expected (samples-per-pixel png)))
    (unless (= samples-per-pixel samples-per-pixel-expected)
      (error 'color-type-mismatch
	     :given samples-per-pixel
	     :expected samples-per-pixel-expected))
    (replace row-data pixel :start1 (current-offset png))
    (when (= (incf (current-offset png) samples-per-pixel) (rowstride png))
      (write-row row-data png)
      (setf (current-offset png) 0)))
  png)

(defmethod pixels-left-in-row ((png pixel-streamed-png))
  (/ (- (current-offset png) (rowstride png)) (samples-per-pixel png)))
