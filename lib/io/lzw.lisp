;;; lzw.lisp --- LZW Compression

;; Based on the LZW compressor in SKIPPY by Zachary Beane

;;; Commentary:

;; The basic LZW compression algorithm is:
;;
;; prefix <- first character
;; while pending data:
;;     char <- next character
;;     if prefix . char in table:
;;         prefix <- prefix . char
;;     else:
;;         output code for prefix
;;         add prefix . char to table
;;         prefix <- char
;; output code for prefix

;;; Code:
(in-package :io/lzw)

(deftype buffer-offset ()
  `(mod ,most-positive-fixnum))

(deftype bitstream-buffer ()
  `(simple-array octet (255)))

;;; bitstream
;; note that IO/DEFLATE also defines a BITSTREAM internally - don't mix the
;; two as they are different protocols..
(declaim (inline bitstream-buffer))
(declaim (inline bitstream-offset))
(declaim (inline bitstream-octet))
(declaim (inline bitstream-count))
(declaim (inline bitstream-bits-left))
(declaim (inline bitstream-stream))
(defstruct (bitstream
             (:constructor
              %make-bitstream (buffer offset count octet bits-left stream)))
  (buffer (make-array 255 :element-type 'octet)
          :type bitstream-buffer)
  (offset 0 :type octet)
  (count 0 :type octet)
  (octet 0 :type octet)
  (bits-left 8 :type (mod 9))
  stream)

(defun make-bitstream (stream)
  (%make-bitstream (make-array 255 :element-type 'octet)
                   0
                   0
                   0
                   8
                   stream))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *bitstream-slot-attributes*
    '((buffer
       :reader bitstream-buffer
       :type bitstream-buffer
       :save nil)
      (offset
       :reader bitstream-offset
       :type octet)
      (count
       :reader bitstream-count
       :type octet)
      (octet
       :reader bitstream-octet
       :type octet)
      (bits-left
       :reader bitstream-bits-left
       :type (mod 9))
      (stream
       :reader bitstream-stream
       :type cl:stream
       :save nil))))

(defmacro with-bitstream-slots (name-bindings bitstream &body body)
  (labels ((binding-var (binding)
             (if (consp binding) (first binding) binding))
           (binding-slot (binding)
             (if (consp binding) (second binding) binding)))
    (let ((type-declarations '())
          (binding-forms '())
          (save-forms '())
          (bitstream-var (gensym)))
      (dolist (binding name-bindings)
        (let* ((var (binding-var binding))
               (slot (binding-slot binding))
               (attributes (cdr (assoc slot *bitstream-slot-attributes*))))
          (unless attributes
            (error "Unknown bitstream slot -- ~S" slot))
          (destructuring-bind (&key reader type (save t))
              attributes
            (push `(,var (,reader ,bitstream-var)) binding-forms)
            (push `(type ,type ,var) type-declarations)
            (when save
              (push `(setf (,reader ,bitstream-var) ,var) save-forms)))))
      `(let ((,bitstream-var ,bitstream))
        (let ,binding-forms
          (declare ,@type-declarations)
          ,@body
          ,@save-forms)))))

(defun reset-stream (bitstream)
  (declare (optimize speed)
           (type bitstream bitstream))
  (with-bitstream-slots (stream buffer offset octet bits-left)
      bitstream
    (when (plusp bits-left)
      (setf (aref buffer offset) octet
            offset (1+ offset)))
    (write-byte offset stream)
    (write-sequence buffer stream :end offset)
    (fill buffer 0)))

(defun write-bits (code length bitstream)
  (declare (type (mod 13) length)
           (type fixnum code)
           (type bitstream bitstream)
           (optimize speed))
  (with-bitstream-slots (stream buffer offset octet bits-left)
      bitstream
    (flet ((merge-bits (len)
             (declare (type (mod 13) len))
             (setf octet (logand #xFF
                                 (logior (ash (ldb (byte len 0) code)
                                              (- 8 bits-left))
                                         octet))
                   bits-left (- bits-left len)
                   code (ash code (- len))
                   length (- length len))))
      (declare (inline merge-bits))
      (loop
       (when (< length bits-left)
         (return))
       (merge-bits bits-left)
       (setf bits-left 8
             (aref buffer offset) octet
             offset (1+ offset)
             octet 0)
       (when (= offset 255)
         (write-byte 255 stream)
         (write-sequence buffer stream)
         (fill buffer 0)
         (setf offset 0)))
      (when (plusp length)
        (merge-bits length)))))

(defun make-input-bitstream (stream)
  (let ((count (read-byte stream))
        (offset 0)
        (buffer (make-array 255 :element-type 'octet))
        (bits-left 0))
    (read-sequence buffer stream :end count)
    (%make-bitstream buffer offset count 0 bits-left stream)))

;;;
;;; When entering and leaving read-bits, OFFSET is always <255 and points
;;; at the NEXT input offset. It is 0 at the start of the process.
;;;
;;; BITS-LEFT may be zero when entering.
;;;

(defun read-bits (length bitstream)
  (declare (type (mod 13) length)
           (type bitstream bitstream)
           (optimize speed))
  (let ((result 0)
        (result-offset 0))
    (declare (type (unsigned-byte 12) result)
             (type (mod 13) result-offset))
    (with-bitstream-slots (stream offset count octet buffer bits-left)
        bitstream
      (loop
       (cond ((< length bits-left)
              (setf result (logior result
                                   (ash (ldb (byte length 0) octet)
                                        result-offset))
                    octet (ash octet (- length))
                    bits-left (- bits-left length))
              (return))
             (t
              (when (= offset count)
                (setf count (read-byte stream)
                      offset 0)
                (read-sequence buffer stream :end count))
                (setf result (logior result (ash octet result-offset))
                      result-offset (+ bits-left result-offset)
                      length (- length bits-left)
                      octet (aref buffer offset)
                      offset (+ offset 1)
                      bits-left 8)))))
    result))

(defun finish-input (bitstream)
  (when (plusp (bitstream-count bitstream))
    (let ((final-block (read-byte (bitstream-stream bitstream))))
      (unless (zerop final-block)
        (warn "Unexpected final block value in stream ~
                      (expected ~D, got ~D)"
              0 final-block)))))

;;; LZW
(defconstant +maximum-code-bits+ 12
  "The maximum bits per code, as defined by the specification.")

(defclass lzw-compressor (compressor)
  ((table
    :initform (make-hash-table)
    :reader table))
  (:documentation
   "Store data structures that may be re-used in an LZW compression context."))

(defun lzw-compress (vector code-size context &optional output)
  (declare (type (simple-array octet (*)) vector)
           (type (mod 13) code-size))
  (let ((iv 0)
        (data-stream (or (output context) (make-bitstream output))))
    (declare (fixnum iv))
    (flet ((next-input ()
             (when (< iv (length vector))
               (prog1
                   (aref vector iv)
                 (incf iv)))))
      (let* ((string-table (table context))
             (clear-code (expt 2 code-size))
             (end-of-input-code (1+ clear-code))
             (index (+ 2 clear-code))
             (compression-size (1+ code-size))
             (max-index (1- (expt 2 compression-size)))
             (prefix (next-input))
             (next-char nil))
        (clrhash string-table)
        (flet ((output-code (code)
                 (write-bits code compression-size data-stream)))
          (output-code clear-code)
          (loop
           (setf next-char (next-input))
           (when (null next-char)
             (output-code prefix)
             (output-code end-of-input-code)
             (reset-stream data-stream)
             (return))
           (let* ((key (logior (ash prefix 8) next-char))
                  (entry (gethash key string-table)))
             (cond (entry
                    (setf prefix entry))
                   (t
                    (output-code prefix)
                    (setf (gethash key string-table) index)
                    (when (> index max-index)
                      (setf max-index (1- (expt 2 (incf compression-size)))))
                    (incf index)
                    (setf prefix next-char))))
           (when (= index #xFFF)
             ;; The index isn't allowed to be this big, so the string
             ;; table must be cleared out and restarted
             (output-code clear-code)
             (setf compression-size (1+ code-size))
             (setf max-index (1- (expt 2 compression-size)))
             (clrhash string-table)
             (setf index (+ 2 clear-code)))))))))

(defmethod compress-octet-vector ((self vector) (compressor lzw-compressor) &key (code-size (length self)) output)
  (lzw-compress self code-size compressor (or output (output compressor))))

(deftype string-table-vector ()
  '(simple-array (signed-byte 16) (4096)))

(deftype string-table-entry ()
  '(signed-byte 16))

(defclass lzw-decompressor (decompressor)
  ((entries
    :initform (make-array (expt 2 +maximum-code-bits+)
                          :element-type 'string-table-entry
                          :initial-element -1)
    :reader entries)
   (preds
    :initform (make-array (expt 2 +maximum-code-bits+)
                          :element-type 'string-table-entry
                          :initial-element -1)
    :reader preds))
  (:documentation
   "A decompression context is used to hold data structures that may
be re-used for repeated calls to lzw-decompress, so they don't have to
be allocated fresh each time."))

(defun lzw-decompress (vector code-size context &optional input)
  "Decompress the GIF LZW data from INPUT into VECTOR."
  (declare (type (simple-array octet (*)) vector)
           (type (mod 9) code-size)
           (optimize speed))
  (let* ((entries (entries context))
         (preds (preds context))
         (clear-code (expt 2 code-size))
         (end-of-input (+ clear-code 1))
         (next-entry-index (+ clear-code 2))
         (compression-size (1+ code-size))
         (compression-threshold (* clear-code 2))
         (last-code -1)
         (pos 0)
         (bitstream (or (input context) (make-input-bitstream input))))
    (declare (type string-table-vector entries preds)
             (type fixnum clear-code end-of-input next-entry-index
                   compression-size compression-threshold
                   last-code pos)
             (type bitstream bitstream))
    (fill entries -1 :start clear-code)
    (fill preds -1)
    (dotimes (i clear-code)
      (setf (aref entries i) i))
    (labels ((reset-table ()
               (when (/= last-code -1)
                 (fill preds -1)
                 (fill entries -1 :start clear-code)
                 (setf last-code -1
                       next-entry-index (+ clear-code 2)
                       compression-size (1+ code-size)
                       compression-threshold (* clear-code 2))))
             (root-value (code)
               (loop
                (let ((pred (aref preds code)))
                  (when (minusp pred)
                    (return (aref entries code)))
                  (setf code pred))))
             (increase-compression-size ()
               (setf compression-size (min +maximum-code-bits+
                                           (+ compression-size 1))
                     compression-threshold (* compression-threshold 2)))
             (add-entry (entry pred)
               (when (> compression-threshold (expt 2 +maximum-code-bits+))
                 (return-from add-entry next-entry-index))
               (when (>= pred next-entry-index)
                 (error 'lzw-error
                        :description "Corrupt data in LZW stream"))
               (let ((result
                      (setf (aref preds next-entry-index) pred
                            (aref entries next-entry-index) entry
                            next-entry-index (1+ next-entry-index))))
                 (when (>= result compression-threshold)
                   (increase-compression-size))
                 (1- result)))
             (code-depth (code)
               (let ((depth 0))
                 (declare (fixnum depth))
                 (loop
                  (let ((pred (aref preds code)))
                    (when (minusp pred)
                      (return depth))
                    (setf depth (1+ depth)
                          code pred)))))
             (output-code-string (code)
               (let ((i (+ pos (code-depth code)))
                     (j pos))
                 (setf pos (1+ i))
                 (when (>= i (length vector))
                   (warn "Too much input data for image, ~
                                 ignoring extra")
                   (finish-input bitstream)
                   (return-from lzw-decompress))
                 (loop
                  (setf (aref vector i) (aref entries code)
                        code (aref preds code)
                        i (- i 1))
                  (when (< i j)
                    (return))))))
      (loop
       (let ((code (read-bits compression-size bitstream)))
         (declare (type fixnum code))
         (cond ((= code clear-code)
                (reset-table))
               ((= code end-of-input)
                (finish-input bitstream)
                (return-from lzw-decompress))
               ((= last-code -1)
                (output-code-string code)
                (setf last-code code))
               (t
                (let ((entry (aref entries code)))
                  (if (minusp entry)
                      (let ((root (root-value last-code)))
                        (output-code-string (add-entry root last-code))
                        (setf last-code code))
                      (let ((root (root-value code)))
                        (add-entry root last-code)
                        (setf last-code code)
                        (output-code-string code)))))))))))

(defmethod decompress-octet-vector ((self vector) (decompressor lzw-decompressor) &key (code-size (length self)) input)
  (lzw-decompress self code-size decompressor (or input (input decompressor))))
