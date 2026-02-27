;;; dat/gif.lisp --- Simple GIF encoding

;; based on SKIPPY by Zachary Beane

;;; Code:
(in-package :dat/gif)

;;; Conditions
(define-condition gif-error (image-error) ())

(define-condition signature-error (gif-error)
  ((source
    :initarg :source
    :reader error-source)
   (position
    :initarg :position
    :initform nil
    :reader error-position)))

(define-condition short-signature (signature-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "Missing signature~@[ at position ~D~] in ~A"
             (error-position condition)
             (error-source condition)))))

(define-condition signature-mismatch (signature-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "Signature mismatch~@[ at position ~D~] in ~A"
             (error-position condition)
             (error-source condition)))))

;;; Vars
(defvar *gif-delay-time* 100)

(defvar *gif-signature*
  (make-array 6
              :element-type '(unsigned-byte 8)
              :initial-contents '(71 73 70 56 57 97))
  "The ASCII codes for the characters of the string \"GIF89a\".")

(defvar *netscape-signature*
  (make-array 11
              :element-type '(unsigned-byte 8)
              :initial-contents '(78 69 84 83 67 65 80 69 50 46 48))
  "The ASCII codes for the characters of the string \"NETSCAPE2.0\".")

(defvar *gif-disposal-methods*
  '((:unspecified . 0)
    (:none . 1)
    (:restore-background . 2)
    (:restore-previous . 3)))

(defconstant +pixel-aspect-ratio+ 0
  "Pixel aspect ratios are not set.")

(defconstant +image-separator-code+ #x2C)

(defconstant +gif-trailer-code+ #x3B
  "The end-of-GIF marker.")

;;; Image
(defclass gif-image (image)
  ((color-table
    :initarg :color-table
    :accessor color-table
    :documentation "The local color table of the image, if any.")
   (transparency
    :initarg :transparency
    :accessor transparency
    :documentation "The color table index of the transparent color for
this image. If null, the image has no transparent color.")
   (interlacedp
    :initarg :interlacedp
    :accessor interlacedp
    :documentation "Is the image interlaced?")
   (disposal-method
    :initarg :disposal-method
    :accessor disposal-method)
   (delay-time
    :initarg :delay-time
    :accessor delay-time
    :documentation "The time, in hundredths of a second, to wait after
this image before displaying the next image"))
  (:default-initargs
   :color-table nil
   :transparency nil
   :interlacedp nil
   :disposal-method nil
   :delay-time *gif-delay-time*))

(defmethod transparentp ((image gif-image))
  (not (null (transparency image))))

(defun make-gif-image (&key height width data stream
                            (top 0) (left 0)
                            color-table
                            interlacedp
                            (delay-time *gif-delay-time*)
                            transparency
                            (disposal-method :unspecified))
  (check-image-dimensions width height)
  (make-instance 'gif-image
    :height height
    :width width
    :data data
    :stream stream
    :top top
    :left left
    :color-table color-table
    :interlacedp interlacedp
    :delay-time delay-time
    :transparency transparency
    :disposal-method disposal-method))

(defmethod initialize-instance :after ((image image)
                                       &key stream
                                       height width
                                       data
                                       color-table
                                       &allow-other-keys)
  (when (eql color-table t)
    (setf (color-table image) (make-color-table)))
  (unless height
    (setf (height image) (height stream)
          height (height stream)))
  (unless width
    (setf (width image) (width stream)
          width (width stream)))
  (cond (data
         (let ((required-type `(array (unsigned-byte 8)
                                (,(* height width)))))
           (unless (typep data required-type)
             (error "Supplied ~S is not of the required type ~A"
                    :data required-type))))
        (t
         (setf (data image) (dat/img::make-image-data height width))))
  (when stream
    (vector-push-extend image (images stream))))

(defun add-delay (delay stream)
  (let ((image (last-image stream)))
    (when image
      (incf (delay-time image) delay))))

(defun deinterlace (canvas)
  (let* ((source (data canvas))
         (dest (copy-seq source))
         (width (width canvas))
         (height (height canvas)))
    (declare (type (simple-array octet (*)) source dest)
             (type fixnum width))
    (flet ((copy-row (i j)
             (let ((s1 (* i width))
                   (s2 (* j width)))
               (replace dest source
                        :start1 s2 :end1 (+ s2 width)
                        :start2 s1))))
      (let ((j -1))
        (macrolet ((pass (start step)
                     `(loop for i from ,start below height by ,step
                       do (copy-row (incf j) i))))
          (pass 0 8)
          (pass 4 8)
          (pass 2 4)
          (pass 1 2))
        dest))))

(defun interlace (canvas)
  (let* ((source (data canvas))
         (dest (copy-seq source))
         (width (width canvas))
         (height (height canvas)))
    (declare (type (simple-array octet (*)) source dest)
             (type fixnum width))
    (flet ((copy-row (i j)
             (let ((s1 (* i width))
                   (s2 (* j width)))
               (replace dest source
                        :start1 s2 :end1 (+ s2 width)
                        :start2 s1))))
      (let ((j -1))
        (macrolet ((pass (start step)
                     `(loop for i from ,start below height by ,step
                       do (copy-row i (incf j)))))
          (pass 0 8)
          (pass 4 8)
          (pass 2 4)
          (pass 1 2))
        dest))))

;;; Stream
(defclass gif-stream (image-stream)
  ((color-table
    :initarg :color-table
    :accessor color-table
    :documentation "The global color table for the data stream (optional)")
   (loopingp
    :initarg :loopingp
    :accessor loopingp)
   (comment
    :initarg :comment
    :accessor comment))
  (:default-initargs
   :color-table nil
   :loopingp nil
   :comment nil))

(defmethod initialize-instance :after ((self gif-stream) 
                                       &key color-table
                                       &allow-other-keys)
  (when (eql color-table t)
    ;; note
    (setf (color-table self) (make-color-table))))

(defmethod (setf stream-of) :after (image (stream gif-stream))
  (unless (slot-boundp image 'height)
    (setf (height image) (height stream)))
  (unless (slot-boundp image 'width)
    (setf (width image) (width stream)))
  (vector-push-extend image (images stream)))

(defun make-gif-stream (&key height width color-table loopingp comment
                             initial-images)
  (let ((self (make-instance 'gif-stream
                :height height
                :width width
                :color-table color-table
                :loopingp loopingp
                :comment comment)))
    (dolist (image initial-images self)
      (add-image image self))))

;;; IO
(defun write-uint16 (number stream)
  (write-byte (logand #xFF number) stream)
  (write-byte (ash number -8) stream))

(defun write-block-terminator (stream)
  (write-byte 0 stream))

(defun boolean-bit (value)
  (if value 1 0))

;;; Spec from http://members.aol.com/royalef/gifabout.htm
(defun write-netscape-looping-block (stream)
  (write-byte #x21 stream)
  (write-byte #xFF stream)
  (write-byte (length *netscape-signature*) stream)
  (write-sequence *netscape-signature* stream)
  (write-byte 3 stream)
  (write-byte 1 stream)
  (write-uint16 #xFFFF stream)
  (write-byte 0 stream))

(defun write-comment (comment stream)
  "Write COMMENT to the GIF. Since the characters must be ASCII,
replace any out-of-range character codes with #\\Space."
  ;;; Comments must be at least one character long
  (when (zerop (length comment))
    (return-from write-comment))
  (when (< 255 (length comment))
    (warn "Truncating comment from ~D to 255 characters"
          (length comment))
    (setf comment (subseq comment 255)))
  (flet ((cleaned-char-code (char)
           (let ((code (char-code char)))
             (if (> code 127) 32 code))))
    (write-byte #x21 stream)
    (write-byte #xFE stream)
    (write-byte (length comment) stream)
    (loop for char across comment do
             (write-byte (cleaned-char-code char) stream))
    (write-block-terminator stream)))

(defun disposal-method-value (keyword)
  (let ((method (assoc keyword *gif-disposal-methods*)))
    (cond (method (cdr method))
          (t
           (warn "Unknown disposal method ~S ~
                         (expected one of ~{~S~^ ~}), using ~S instead"
                 keyword
                 (mapcar #'car *gif-disposal-methods*)
                 :unspecified)
           0))))

(defun write-graphic-control-block (image stream)
  (let ((extension-introducer #x21)
        (graphic-control-label #xF9)
        (block-size 4))
    (write-byte extension-introducer stream)
    (write-byte graphic-control-label stream)
    (write-byte block-size stream)
    ;; packed field: RRRDDDUT
    ;; RRR = reserved (left as zero)
    ;; DDD = disposal method
    ;; U = user input (ignored, left as zero),
    ;; T = transparent color flag
    (let ((flags 
            (logior 
             (dpb (disposal-method-value (disposal-method image))
                  (byte 3 2)
                  0)
             (dpb (boolean-bit (transparentp image))
                  (byte 1 0)
                  0))))
      (write-byte flags stream))
    (write-uint16 (delay-time image) stream)
    (write-byte (or (transparency image) 0) stream)
    (write-block-terminator stream)))

(defun write-color-table (table stream)
  (let ((count (expt 2 (color-table-code-size table))))
    (loop for color across table
          do (multiple-value-bind (r g b)
                 (color-rgb color)
               (write-byte r stream)
               (write-byte g stream)
               (write-byte b stream))
             (decf count))
    (dotimes (i (* count 3))
      (write-byte 0 stream))))

(defun effective-color-table (image)
  "Return the color table in effect when writing out IMAGE, or signal
an error if no color table is available."
  (let ((color-table (color-table image)))
    (cond (color-table)
          ((or (not (slot-boundp image 'stream))
               (not (stream-of image)))
           (error 'missing-color-table image))
          ((color-table (stream-of image)))
          (t
           (error 'missing-color-table image)))))

(defun compression-code-size (image)
  "Return the number of bits needed to represent the largest index in
the effective color table of INDEX."
  (color-table-code-size (effective-color-table image)))

(defun write-gif-image (image context stream)
  (let* ((color-table (color-table image))
         (code-size (compression-code-size image))
         (width (width image))
         (height (height image)))
    (check-image-dimensions width height)
    (write-graphic-control-block image stream)
    (write-byte +image-separator-code+ stream)
    (write-uint16 (left image) stream)
    (write-uint16 (top image) stream)
    (write-uint16 (width image) stream)
    (write-uint16 (height image) stream)
    ;; packed byte: CISRRSSS
    ;; C = local color table flag
    ;; I = interlaced flag (left as zero)
    ;; S = sort flag (left as zero)
    ;; RR = reserved (left as zero)
    ;; SSS = size (bit depth) of color table, minus one
    (let ((flags
            (logior
             (dpb (boolean-bit color-table)         (byte 1 7) 0)
             (dpb (boolean-bit (interlacedp image)) (byte 1 6) 0)
             (dpb (1- code-size)                    (byte 3 0) 0))))
      (write-byte flags stream))
    (when color-table
      (write-color-table color-table stream))
    (write-byte code-size stream)
    (let ((data (if (interlacedp image)
                    (interlace image)
                    (data image))))
      (compress-octet-vector data context :code-size code-size :output stream))
    (write-block-terminator stream)))

(defun write-gif-stream-header (data-stream stream)
  (let* ((color-table (color-table data-stream))
         (code-size (color-table-code-size color-table)))
    (write-sequence *gif-signature* stream)
    (write-uint16 (width data-stream) stream)
    (write-uint16 (height data-stream) stream)
    ;; packed byte: GRRRSTTT
    ;; G = global color table flag, RRR = color resolution, S = sort flag,
    ;; TTT = global color table size
    (write-byte (logior (ash (boolean-bit (color-table data-stream)) 7)
                        (1- code-size))
                stream)
    ;; background color index
    (write-byte 0 stream)
    (write-byte +pixel-aspect-ratio+ stream)
    (when color-table
      (write-color-table color-table stream))
    (when (comment data-stream)
      (write-comment (comment data-stream) stream))
    (when (loopingp data-stream)
      (write-netscape-looping-block stream))))

(defun write-end-code (data-stream stream)
  (declare (ignore data-stream))
  (write-byte +gif-trailer-code+ stream))

(defun write-gif-stream (data-stream stream)
  (write-gif-stream-header data-stream stream)
  (when (zerop (length (images data-stream)))
    (warn "No images in ~A" data-stream))
  (loop with context = (make-instance 'lzw-compressor :output (io/lzw::make-bitstream stream))
        for image across (images data-stream) do
           (check-dimensions data-stream image)
           (write-gif-image image context stream))
  (write-end-code data-stream stream)
  (values))

(defun output-gif-stream (data-stream file &key (if-exists :supersede))
  (with-open-file (stream file
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists if-exists)
    (write-gif-stream data-stream stream)
    (probe-file file)))

(defmethod serialize ((self gif-stream) (format (eql :gif)) &key path (if-exists :supersede))
  (output-gif-stream self path :if-exists if-exists))
(defmethod serde ((from gif-stream) (to pathname))
  (output-gif-stream from to))

;;; Loader
(defvar *effective-graphic-control* nil
  "The graphic control extension in effect for the current image.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +extension-introducer+ #x21)
  (defconstant +graphic-control-label+ #xF9)
  (defconstant +comment-label+ #xFE)
  (defconstant +application-label+ #xFF) 
  (defconstant +plain-text-label+ #x01))

(defclass graphic-control-extension ()
  ((delay-time
    :initarg :delay-time
    :reader delay-time)
   (disposal-method
    :initarg :disposal-method
    :reader disposal-method)
   (transparency-index
    :initarg :transparency-index
    :reader transparency-index)))

(defmacro bind-bits ((integer size) bindings &body body)
  (let ((value (gensym))
        (names (mapcar #'first bindings))
        (sizes (mapcar #'second bindings)))
    (let ((total-size (apply #'+ sizes)))
      (when (> total-size size)
        (error "Bitfield total size (~D) is larger than provided integer size (~D)"
               total-size size))
      `(let* ((,value ,integer)
              ,@(loop for offset = size then (- offset field-size)
                      for name in names
                      for field-size in sizes
                      when name
                      collect (list name
                                    `(ldb (byte ,field-size ,(- offset field-size)) ,value))))
         ,@body))))

(defun read-uint16 (stream)
  (logand #xFFFF (+ (ash (read-byte stream) 0)
                    (ash (read-byte stream) 8))))

(defun read-color (stream)
  (logand #xFFFFFF (+ (ash (read-byte stream) 16)
                      (ash (read-byte stream)  8)
                      (ash (read-byte stream)  0))))

(defun read-color-table (count stream)
  (let ((color-table (make-color-table)))
    (dotimes (i count color-table)
      (add-color (read-color stream) color-table))))

(defun stream-position (stream &key (offset 0))
  "FILE-POSITION may return NIL or may signal an error \(for e.g. Gray
streams); wrap it."
  (let ((pos (ignore-errors (file-position stream))))
    (when pos
      (+ pos offset))))

(defun advance-stream-position (stream count)
  "Skip past COUNT bytes of input in STREAM."
  (let ((pos (stream-position stream :offset count)))
    (if pos
        (file-position stream pos)
        (dotimes (i count)
          (read-byte stream)))))

(defun merge-graphic-control (image)
  (when *effective-graphic-control*
    (setf (delay-time image)
          (delay-time *effective-graphic-control*)
          (disposal-method image)
          (disposal-method *effective-graphic-control*)
          (transparency image)
          (transparency *effective-graphic-control*)
          *effective-graphic-control* nil)))

(defun read-gif-image (context stream)
  (let ((left-position (read-uint16 stream))
        (top-position (read-uint16 stream))
        (width (read-uint16 stream))
        (height (read-uint16 stream))
        (flags (read-byte stream))
        (color-table nil))
    (bind-bits (flags 8)
               ((local-color-table-flag 1)
                (interlaced-flag        1)
                (sort-flag              1)
                (reserved               2)
                (color-table-size       3))
      (declare (ignore sort-flag reserved))
      (when (plusp local-color-table-flag)
        (let ((color-table-entry-count (expt 2 (1+ color-table-size))))
          (setf color-table (read-color-table color-table-entry-count
                                              stream))))
      (let* ((code-size (read-byte stream))
             (image-data (dat/img::make-image-data width height)))
        (decompress-octet-vector image-data context :input stream :code-size code-size)
        (let ((image 
                (make-gif-image :left left-position
                                :top top-position
                                :width width
                                :height height
                                :data image-data
                                :color-table color-table
                                :interlacedp (plusp interlaced-flag))))
          (when (plusp interlaced-flag)
            (replace image-data (deinterlace image)))
          (merge-graphic-control image)
          image)))))

(defun disposal-method-keyword (method)
  (or (car (rassoc method *gif-disposal-methods*))
      :unspecified))

(defun read-graphic-control-extension (stream)
  ;; STREAM is positioned just after the Graphic Control Label
  (let ((block-size (read-byte stream)))
    (when (/= block-size 4)
      (error 'unexpected-value
             :description "block-size"
             :expected-value 4
             :actual-value block-size
             :source stream
             :source-position (stream-position stream :offset -1)))
    (let ((fields (read-byte stream))
          (delay-time (read-uint16 stream))
          (transparency-index (read-byte stream))
          (block-terminator (read-byte stream)))
      (when (/= block-terminator 0)
        (error 'unexpected-value
               :description "block-terminator"
               :actual-value block-terminator
               :expected-value 0
               :source stream
               :source-position (stream-position stream :offset -1)))
      (bind-bits (fields 8)
                 ((reserved               3)
                  (disposal-method        3)
                  (user-input-flag        1)
                  (transparent-color-flag 1))
        (declare (ignore reserved user-input-flag))
        (when (zerop transparent-color-flag)
          (setf transparency-index nil))
        (make-instance 'graphic-control-extension
          :delay-time delay-time
          :disposal-method (disposal-method-keyword disposal-method)
          :transparency-index transparency-index)))))

(defun skip-data-blocks (stream)
  ;; Data blocks take the form of a series of (<size octet> <vector of
  ;; <size octet>s of data>) sequences. A size octet of zero
  ;; terminates a data block.
  (loop
    (let ((size (read-byte stream)))
      (when (zerop size)
        (return))
      (advance-stream-position stream size))))

(defun read-application-extension (stream data-stream)
  (let ((block-size (read-byte stream)))
    (let ((block (make-array block-size :element-type 'octet)))
      (read-sequence block stream)
      ;;; XXX If we ever supports more application extensions, it
      ;;; would make sense to put them in a table instead of
      ;;; hardcoding specific extension identifiers here.
      (when (equalp block *netscape-signature*)
        (setf (loopingp data-stream) t)))
    (skip-data-blocks stream)))

(defun read-comment-extension (stream)
  (flet ((ascii-char (code)
           ;;; FIXME: This assumes ASCII code-char mapping; could keep a table
           ;;; instead.
           (code-char (min code 127))))
    (with-output-to-string (output)
      (let ((block (make-array 255 :element-type 'octet)))
        (loop
          (let ((count (read-byte stream)))
            (when (zerop count)
              (return))
            (read-sequence block stream :end count)
            (loop for i below count
                  for octet across block
                  do (write-char (ascii-char octet) output))))))))

(defun read-extension-object (stream data-stream)
  (let ((label (read-byte stream)))
    (case label
      (#.+plain-text-label+
       (skip-data-blocks stream))
      (#.+graphic-control-label+
       (setf *effective-graphic-control*
             (read-graphic-control-extension stream)))
      (#.+application-label+
       (read-application-extension stream data-stream))
      (#.+comment-label+
       (when (comment data-stream)
         (warn "Multiple comments found; only the final comment ~
                       will be loaded"))
       (setf (comment data-stream) (read-comment-extension stream)))
      (t
       (warn "Skipping unrecognized extension with label #x~2,'0X" label)
       (skip-data-blocks stream)))))

(defun process-objects (data-stream stream)
  (let ((context (make-instance 'lzw-decompressor)))
    (loop
      (let ((tag (read-byte stream nil)))
        (case tag
          ((nil)
           (return))
          (#.+gif-trailer-code+
           (return))
          (#.+image-separator-code+
           (add-image (read-gif-image context stream) data-stream))
          (#.+extension-introducer+
           (read-extension-object stream data-stream))
          (t
           (warn "Unknown tag ~D in ~A~:[~; at position ~:*~D~]"
                 tag stream (stream-position stream :offset -1))))))))

(defvar *gif87a-signature*
  ;; The ASCII for string "GIF87a"
  (make-array 6 :element-type 'octet
                :initial-contents #(71 73 70 56 55 97)))

(defvar *gif89a-signature*
  ;; The ASCII for string "GIF89a"
  (make-array 6 :element-type 'octet
                :initial-contents #(71 73 70 56 57 97)))

(defun check-gif-signature (stream)
  "Check that STREAM starts with the ASCII string \"GIF89a\" or \"GIF87a\"."
  (let* ((pos (stream-position stream))
         (signature (make-array 6 :element-type 'octet))
         (count (read-sequence signature stream)))
    (when (/= count 6)
      (error 'short-signature
             :source stream
             :position pos))
    (when (and (mismatch signature *gif89a-signature*)
               (mismatch signature *gif87a-signature*))
      (error 'signature-mismatch
             :source stream
             :position pos))))

(defun read-gif-stream (stream)
  (check-gif-signature stream)
  (let ((width (read-uint16 stream))
        (height (read-uint16 stream))
        (flags (read-byte stream))
        (background-color-index (read-byte stream))
        (pixel-aspect-ratio (read-byte stream))
        (color-table nil)
        (*effective-graphic-control* nil))
    (declare (ignore background-color-index pixel-aspect-ratio))
    (bind-bits (flags 8)
               ((global-color-table-flag 1)
                (color-resolution        3)
                (sorted-flag             1)
                (global-color-table-size 3))
      (declare (ignore color-resolution sorted-flag))
      (when (plusp global-color-table-flag)
        (let ((color-table-entry-count (expt 2 (1+ global-color-table-size))))
          (setf color-table (read-color-table color-table-entry-count
                                              stream))))
      (let ((data-stream (make-gif-stream :height height
                                          :width width
                                          :color-table color-table)))
        (process-objects data-stream stream)
        data-stream))))

(defun load-gif-stream (file)
  (with-open-file (stream file :direction :input :element-type 'octet)
    (read-gif-stream stream)))
