;;; img.lisp --- Shared Image Data Protocol

;; Based on Zachary Beane's SKIPPY gif encoder.

;; The IMAGE class is based on the CANVAS class which is the lisp-native,
;; low-level representation of a 2D graphic. We implement the same io protocol
;; as SKIPPY on CANVAS objects directly - specifically the
;; READ/WRITE/LOAD/SAVE-CANVAS functions, but the IMAGE protocol is
;; generalized and implemented by our downstream DAT image packages - notably
;; DAT/PNG, DAT/SVG, DAT/JPEG, and DAT/GIF.

;;; Code:
(in-package :dat/img)

(deftype image-dimension ()
  `(integer 1 #xFFFF))

(deftype canvas-data ()
  '(simple-array (unsigned-byte 8) (*)))

(deftype canvas-index ()
  `(mod ,most-positive-fixnum))

(define-condition image-condition () ())
(define-condition image-error (image-condition error) ())

(define-condition invalid-image-dimensions (image-error)
  ((width
    :initarg :width
    :reader error-width)
   (height
    :initarg :height
    :reader error-height))
  (:report
   (lambda (condition stream)
     (format stream "Invalid image dimensions ~Ax~A - each dimensions must ~
                     be (< 0 dimension 65536)"
             (error-width condition)
             (error-height condition)))))

(defclass canvas ()
  ((height :initarg :height :reader height)
   (width :initarg :width :reader width)
   (data :initarg :data :reader data))
  (:default-initargs
   :height (required-argument :height)
   :width (required-argument :width)))

(defmethod print-object ((canvas canvas) stream)
  (print-unreadable-object (canvas stream :type t :identity t)
    (format stream "~Dx~D" (width canvas) (height canvas))))

(defclass image (canvas io-stream wrapped-stream)
  ((height :initarg :height :accessor height)
   (width :initarg :width :accessor width)
   (data :initarg :data :accessor data)
   (top
    :initarg :top
    :accessor top
    :documentation
    "The position of the image relative to the top of the logical screen")
   (left
    :initarg :left
    :accessor left
    :documentation
    "The position of the image relative to the left of the logical screen"))
  (:default-initargs
   :top 0
   :left 0
   :height nil
   :width nil))

(defmethod print-object ((object image) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "~Dx~D+~D+~D"
            (width object)
            (height object)
            (left object)
            (top object))))

(defgeneric pixel (self x y))
(defgeneric (setf pixel) (new self x y))
(defgeneric rotate-180 (self))
(defgeneric flip-horizontal (self))
(defgeneric flip-vertical (self))
(defgeneric scale (self factor))
(defgeneric fill-area (canvas idx &key x y width height))
(defgeneric image (self))
(defgeneric (setf image) (new self))
(defgeneric images (self))
(defgeneric (setf images) (new self))
(defgeneric transparentp (image))

;;; canvas
(defmethod clone ((canvas canvas))
  (make-instance 'canvas
                 :height (height canvas)
                 :width (width canvas)
                 :data (copy-seq (data canvas))))

(defun fill-canvas (canvas idx)
  (declare (type octet idx)
           (optimize (speed 3)))
  (let ((data (data canvas)))
    (declare (type canvas-data data))
    (fill data idx)
    (values)))

(defun check-image-dimensions (width height)
  (unless (and (typep width 'image-dimension)
               (typep height 'image-dimension))
    (error 'invalid-image-dimensions
           :width width
           :height height)))

(defun make-image-data (width height &key
                        (initial-element 0)
                        initial-contents)
  (if initial-contents
      (make-array (* width height)
                  :element-type 'octet
                  :initial-contents initial-contents)
      (make-array (* width height)
                  :element-type 'octet
                  :initial-element initial-element)))

(defun make-canvas (&key width height
                    data (initial-element 0) initial-contents)
  (unless (and height width)
    (error "~S and ~S required" :height :width))
  (unless data
    (setf data (make-image-data width height
                                      :initial-element initial-element
                                      :initial-contents initial-contents)))
  (make-instance 'canvas
                 :height height
                 :width width
                 :data data))

(defmethod initialize-instance :after ((canvas canvas) &key height width)
  (unless (slot-boundp canvas 'data)
    (setf (data canvas) (make-array (* height width)
                                          :initial-element 0
                                          :element-type 'octet))))

(defun clip (xmin0 ymin0 xmax0 ymax0
             xmin1 ymin1 xmax1 ymax1)
  (flet ((clamp (min val max)
           (cond ((< val min) min)
                 ((> val max) max)
                 (t val))))
    (values (clamp xmin0 xmin1 xmax0)
            (clamp ymin0 ymin1 ymax0)
            (clamp xmin0 xmax1 xmax0)
            (clamp ymin0 ymax1 ymax0))))

(defun clip-canvas (source dest &key (sx 0) (sy 0) (dx 0) (dy 0)
                   (width (width source)) (height (height source)))
  "Return new dx,dy and sx,sy and width,height values to use when
clipping SOURCE to fit within the bounds of DEST."
  (let* ( ;; destination
         (xmin0 0)
         (ymin0 0)
         (xmax0 (width dest))
         (ymax0 (height dest))
         ;; source
         (xmin1 (- dx sx))
         (ymin1 (- dy sy))
         (xmax1 (+ xmin1 (width source)))
         (ymax1 (+ ymin1 (height source)))
         ;; source offset
         (xmin2 dx)
         (ymin2 dy)
         (xmax2 (+ xmin2 width))
         (ymax2 (+ ymin2 height)))
    ;; clip source offset to source
    (multiple-value-bind (xmin3 ymin3 xmax3 ymax3)
        (clip xmin1 ymin1 xmax1 ymax1
              xmin2 ymin2 xmax2 ymax2)
      ;; clip that against dest
      (multiple-value-bind (xmin4 ymin4 xmax4 ymax4)
          (clip xmin0 ymin0 xmax0 ymax0
                xmin3 ymin3 xmax3 ymax3)
        (values xmin4 ymin4
                (- xmin4 xmin1)
                (- ymin4 ymin1)
                (- xmax4 xmin4)
                (- ymax4 ymin4))))))

(defun composite (source dest
                  &key (sx 0) (sy 0)
                  (dx 0) (dy 0)
                  (width (width source)) (height (height source)))
  (multiple-value-bind (dx* dy* sx* sy* width* height*)
      (clip-canvas source dest
                   :sx sx :sy sy
                   :dx dx :dy dy
                   :width width :height height)
    (when (or (zerop width*)
              (zerop height*))
      (return-from composite))
    (let ((source-data (data source))
          (source-width (width source))
          (dest-data (data dest))
          (dest-width (width dest)))
      (declare (type canvas-data source-data dest-data)
               (type canvas-index source-width dest-width))
      (loop repeat height*
            for source-start from (+ (* source-width sy*) sx*) by source-width
            for dest-start   from (+ (* dest-width   dy*) dx*) by dest-width
            for source-end   from (+ source-start width*) by source-width
            do (replace dest-data source-data :start1 dest-start
                        :start2 source-start :end2 source-end))
      dest)))

(defmethod flip-horizontal ((canvas canvas))
  "Horizontally mirror the image data of CANVAS."
  (loop repeat (height canvas)
        with data = (data canvas)
        with width = (width canvas)
        for i = 0 then (+ i width)
        for j = (1- width) then (+ j width)
        do (loop for m from i
                 for n downfrom j
                 while (< m n) do
                 (rotatef (aref data m) (aref data n))))
  canvas)

(defmethod rotate-180 (canvas)
  "Does a 180-degree rotation of the image data of CANVAS."
  (setf (data canvas) (nreverse (data canvas)))
  canvas)

(defmethod flip-vertical (canvas)
  "Vertically mirror the image data of CANVAS."
  (rotate-180 canvas)
  (flip-horizontal canvas))

(defmethod scale ((canvas canvas) factor)
  "Integer scale CANVAS and return it as a new canvas." 
  (let* ((width (* (width canvas) factor))
         (height (* (height canvas) factor))
         (new (make-instance 'canvas :width width :height height)))
    (dotimes (y (height canvas) new)
      (dotimes (x (width canvas))
        (let ((p (pixel canvas x y))
              (xf (* x factor))
              (yf (* y factor)))
          (dotimes (i factor)
            (dotimes (j factor)
              (setf (pixel new (+ xf i) (+ yf j)) p))))))))

(defmethod fill-area (canvas idx &key
                      (x 0)
                      (y 0)
                      (width (width canvas))
                      (height (height canvas)))
  (let ((xmin0 x)
        (ymin0 y)
        (xmax0 (+ x width))
        (ymax0 (+ y height))
        (xmin1 0)
        (ymin1 0)
        (xmax1 (width canvas))
        (ymax1 (height canvas)))
    (multiple-value-bind (xmin2 ymin2 xmax2 ymax2)
        (clip xmin0 ymin0 xmax0 ymax0
              xmin1 ymin1 xmax1 ymax1)
      (let ((w (- xmax2 xmin2))
            (h (- ymax2 ymin2)))
        (when (and (plusp w) (plusp h))
          (loop with dest-width = (width canvas)
                with data = (data canvas)
                with start = (+ xmin2 (* ymin2 dest-width))
                for i = start then (+ i dest-width)
                for j = (+ start w) then (+ j dest-width)
                repeat h
                do (fill data idx :start i :end j)))))))

(defmethod pixel (canvas x y)
  (aref (data canvas) (+ (* y (width canvas)) x)))

(defmethod (setf pixel) (new-value canvas x y)
  (setf (aref (data canvas) (+ (* y (width canvas)) x)) new-value))

(defvar *canvas-magic*
  (make-array 3 :element-type '(unsigned-byte 8)
              :initial-contents (list #x89 #xAD #x17)))

(defvar *file-format-version* 1)

(defun write-u32 (i stream)
  (write-byte (logand #xFF (ash i -24)) stream)
  (write-byte (logand #xFF (ash i -16)) stream)
  (write-byte (logand #xFF (ash i  -8)) stream)
  (write-byte (logand #xFF (ash i   0)) stream))

(defun read-u32 (stream)
  (logand #xFFFFFFFF
          (+ (ash (read-byte stream) 24)
             (ash (read-byte stream) 16)
             (ash (read-byte stream)  8)
             (ash (read-byte stream)  0))))

(defun write-canvas (canvas stream)
  (write-sequence *canvas-magic* stream)
  (write-byte *file-format-version* stream)
  (write-u32 (width canvas) stream)
  (write-u32 (height canvas) stream)
  (write-sequence (data canvas) stream)
  t)

(defun read-canvas (stream)
  (dotimes (i (length *canvas-magic*))
    (let ((byte (read-byte stream)))
      (when (/= byte (aref *canvas-magic* i))
        (error "Bad magic in stream"))))
  (let ((version (read-byte stream)))
    (when (/= version *file-format-version*)
      (error "Unsupported version in stream -- expected ~D, read ~D"
             *file-format-version* version)))
  (let ((width (read-u32 stream))
        (height (read-u32 stream)))
    (when (>= (* width height) array-total-size-limit)
      (error "Canvas dimensions (~Dx~D) too large to load"
             width height))
    (let ((canvas (make-instance 'canvas :height height :width width)))
      (read-sequence (data canvas) stream)
      canvas)))

(defun save-canvas (canvas file &key (if-exists :supersede))
  (with-open-file (stream file :element-type '(unsigned-byte 8)
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists if-exists)
    (write-canvas canvas stream))
  (probe-file file))

(defun load-canvas (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8)
                          :direction :input)
    (read-canvas stream)))

(defclass image-stream (io-stream)
  ((height
    :initarg :height
    :reader height
    :documentation "The height of the logical screen")
   (width
    :initarg :width
    :reader width
    :documentation "The width of the logical screen")
   (images
    :initarg :images
    :reader images
    :documentation "A vector of the images in the data stream"))
  (:default-initargs
   :height (required-argument :height)
   :width (required-argument :width)
   :images (make-array 10 :adjustable t :fill-pointer 0))
  (:documentation
   "An IMAGE-STREAM instance represents a container for image
data. It defines the logical dimensions of the overall image."))

(defmethod print-object ((object image-stream) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~Dx~D, ~D image~:*~P"
            (width object)
            (height object)
            (length (images object)))))

(defun last-image (stream)
  (let* ((images (images stream))
         (i (fill-pointer images)))
    (unless (zerop i)
      (aref images (1- i)))))

(defun check-dimensions (stream image)
  (when (or (< (height stream) (height image))
            (< (width stream) (width image)))
    (warn "Image ~A is larger than its containing stream ~A, ~
                  output may not display properly"
          image stream)))

(defun add-image (image stream)
  (setf (stream-of image) stream)
  (check-dimensions stream image)
  (vector-push-extend image (images stream)))
