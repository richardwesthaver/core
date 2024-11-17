;;; static.lisp --- Static Storage IO

;; Vectors allocated in static memory. Useful for things like IO buffers
;; created from Lisp and shared with C code.

;;; Commentary:

;; The source here is pulled directly from the STATIC-VECTORS package on
;; Quicklisp: https://github.com/sionescu/static-vectors

;;; Code:
(in-package :io/static)
;;; --- Checking for compile-time constants and evaluating such forms

(defun quotedp (form)
  (and (listp form)
       (= 2 (length form))
       (eql 'quote (car form))))

(defun qconstantp (form &optional env)
  (let ((form (if (symbolp form)
                  (macroexpand form env)
                  form)))
    (or (quotedp form)
        (cl:constantp form))))

(defun eval-constant (form &optional env)
  (declare (ignorable env))
  (if (quotedp form)
      (second form)
      (sb-int:constant-form-value form env)))

(defun canonicalize-args (env element-type length)
  (let* ((eltype-spec (or (and (qconstantp element-type)
                               (ignore-errors
                                (upgraded-array-element-type
                                 (eval-constant element-type))))
                          '*))
         (length-spec (if (qconstantp length env)
                          `,(eval-constant length env)
                          '*))
         (type-decl (if (eql '* eltype-spec)
                        'simple-array
                        `(simple-array ,eltype-spec (,length-spec)))))
    (values (if (eql '* eltype-spec)
                element-type
                `(quote ,eltype-spec))
            (if (eql '* length-spec)
                length
                length-spec)
            type-decl)))

;;; --- SBCL implementation
(declaim (inline fill-foreign-memory))
(defun fill-foreign-memory (pointer length value)
  "Fill LENGTH octets in foreign memory area POINTER with VALUE."
  (std/alien:memset pointer value length)
  pointer)

(declaim (inline replace-foreign-memory))
(defun replace-foreign-memory (dst-ptr src-ptr length)
  "Copy LENGTH octets from foreign memory area SRC-PTR to DST-PTR."
  (std/alien:memcpy dst-ptr src-ptr length)
  dst-ptr)

;;; We have to handle all the low-level bits including setting the array header
;;; and keeping around the info about the original pointer returned by the
;;; foreign allocator.
;;;
;;; It goes like this:
;;;
;;; 1. Compute the data size for the Lisp-visible memory (that means an extra #\Nul
;;;    at the end for strings)
;;; 2. Sum the data size, the SBCL header size, and our extra header size to get
;;;    the total foreign size required
;;; 3. Adjust the total foreign size to the required alignment, compute the header offset
;;;    and write the headers.
;;;
;;; Array layout:
;;;
;;;    +-------------------+
;;;    | Allocated address | <-- Original pointer
;;;    +-------------------+
;;;    | Start gap ...     | <-- For large alignments, there's a gap between
;;;    |                   |     the data block and the headers.
;;;    +-------------------+
;;;    | SV header         | <-- The offset from the original pointer (DWORD)
;;;    +-------------------+
;;;    | Lisp array header | <-- Array element-type and size (DWORD)
;;;    +-------------------+
;;;    | Lisp array data   | <-- Lisp-visible data
;;;    +-------------------+
;;;
;;; There's no end gap because when a alignment is requested,
;;; the requested size must also be a multiple of the alignment.

(defconstant +array-header-size+
  (* sb-vm:vector-data-offset sb-vm:n-word-bytes))

(declaim (inline vector-widetag-and-n-bytes))
(defun vector-widetag-and-n-bytes (type)
  "Returns the widetag and octet size of the upgraded array element type
for a given type specifier."
  (let ((upgraded-type (upgraded-array-element-type type)))
    (case upgraded-type
      ((nil t) (error "~A is not a specializable array element type" type))
      (t
       #+#.(cl:if (cl:find-symbol "%VECTOR-WIDETAG-AND-N-BITS" "SB-IMPL")
                  '(and) '(or))
       (sb-impl::%vector-widetag-and-n-bits type)
       #+#.(cl:if (cl:find-symbol "%VECTOR-WIDETAG-AND-N-BITS-SHIFT" "SB-IMPL")
                  '(and) '(or))
       (multiple-value-bind (widetag shift)
           (sb-impl::%vector-widetag-and-n-bits-shift type)
         (values widetag (expt 2 (- shift 3))))))))

(declaim (inline align))
(defun align (size boundary)
  (* boundary
     (ceiling size boundary)))

(declaim (inline %memalign))
(defun %memalign (size alignment)
  (with-alien ((box (* t)))
    (let ((errno 
            (std/alien:posix-memalign (addr box) alignment size)))
      (when (not (zerop errno))
        (error "posix_memalign() returned error ~A" errno))
      box)))

(defun %allocate-static-vector (length element-type alignment)
  (declare (type (unsigned-byte 16) alignment))
  (flet ((allocation-sizes (length widetag n-bytes)
           (values
            ;; We're allocating two headers: one for SBCL and
            ;; the other one for our bookkeeping.
            (align (* 2 +array-header-size+) alignment)
            ;; Align data size.
            (align
             (* (if (= widetag sb-vm:simple-character-string-widetag)
                    (1+ length)         ; for the final #\Nul
                    length)
                n-bytes)
             alignment))))
    (multiple-value-bind (widetag n-bytes)
        (vector-widetag-and-n-bytes element-type)
      (multiple-value-bind (header-size data-size)
          (allocation-sizes length widetag n-bytes)
        (let* ((total-size (+ header-size data-size))
               (foreign-block (%memalign total-size alignment))
               (data-offset header-size )
               (lisp-header-offset
                 (- data-offset +array-header-size+))
               (lisp-header-pointer
                 (sb-sys:sap+ (alien-sap foreign-block) lisp-header-offset))
               (extra-header-offset
                 (- data-offset (* 2 +array-header-size+)))
               (extra-header-pointer
                 (sb-sys:sap+ (alien-sap foreign-block) extra-header-offset)))
          ;; Write Lisp header: tag and length
          (setf (sb-sys:sap-ref-word lisp-header-pointer 0) widetag)
          (setf (sb-sys:sap-ref-word lisp-header-pointer sb-vm:n-word-bytes)
                (sb-vm:fixnumize length))
          ;; Save the relative position from the start of the foreign block
          (setf (sb-sys:sap-ref-word extra-header-pointer 0)
                (- data-offset (* 2 +array-header-size+)))
          ;; Instantiate Lisp object
          (sb-kernel:%make-lisp-obj (logior (sb-sys:sap-int lisp-header-pointer)
                                            sb-vm:other-pointer-lowtag)))))))

(declaim (inline static-vector-address))
(defun static-vector-address (vector)
  "Return a foreign pointer to start of the Lisp VECTOR(including its header).
VECTOR must be a vector created by MAKE-STATIC-VECTOR."
  (logandc2 (sb-kernel:get-lisp-obj-address vector)
            sb-vm:lowtag-mask))

(declaim (inline static-vector-pointer))
(defun static-vector-pointer (vector &key (offset 0))
  "Return a foreign pointer to the beginning of VECTOR + OFFSET octets.
VECTOR must be a vector created by MAKE-STATIC-VECTOR."
  (check-type offset unsigned-byte)
  (sb-sys:int-sap (+ (static-vector-address vector)
                   +array-header-size+
                   offset)))

(declaim (inline free-static-vector))
(defun free-static-vector (vector)
  "Free VECTOR, which must be a vector created by MAKE-STATIC-VECTOR."
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let* ((extra-header-pointer
           (sb-sys:int-sap (- (static-vector-address vector) +array-header-size+)))
         (start-offset
           (sb-sys:sap-ref-word extra-header-pointer 0)))
    (free-alien (sap-alien (sb-sys:sap+ extra-header-pointer (- start-offset)) (* (unsigned 8)))))
  (values))

;;; --- MAKE-STATIC-VECTOR
(declaim (inline check-initial-element))
(defun check-initial-element (element-type initial-element)
  (when (not (typep initial-element element-type))
    (error "MAKE-STATIC-VECTOR: The type of :INITIAL-ELEMENT ~S is not a subtype ~
of the array's :ELEMENT-TYPE ~S"
           initial-element element-type)))

(declaim (inline check-initial-contents))
(defun check-initial-contents (length initial-contents)
  (let ((initial-contents-length (length initial-contents)))
    (when (/= length initial-contents-length)
      ;; FIXME: signal TYPE-ERROR
      (error "MAKE-STATIC-VECTOR: There are ~A elements in the :INITIAL-CONTENTS, ~
but requested vector length is ~A."
             initial-contents-length length))))

(declaim (inline check-initialization-arguments))
(defun check-initialization-arguments (initial-element-p initial-contents-p)
  (when (and initial-element-p initial-contents-p)
    ;; FIXME: signal ARGUMENT-LIST-ERROR
    (error "MAKE-STATIC-VECTOR: You must not specify both ~
:INITIAL-ELEMENT and :INITIAL-CONTENTS")))

(defun check-arguments (length element-type
                        initial-element initial-element-p
                        initial-contents initial-contents-p)
  (check-initialization-arguments initial-element-p initial-contents-p)
  (check-type length non-negative-fixnum)
  (when initial-element-p
    (check-initial-element element-type initial-element))
  (when initial-contents-p
    (check-initial-contents length initial-contents)))

(defconstant +default-alignment+ 16)
(defconstant +max-alignment+ 4096)

(declaim (inline make-static-vector))
(defun make-static-vector (length &key (element-type '(unsigned-byte 8))
                           (initial-element nil initial-element-p)
                           (initial-contents nil initial-contents-p)
                           (alignment nil alignp))
  "Create a simple vector of length LENGTH and type ELEMENT-TYPE which will
not be moved by the garbage collector. The vector might be allocated in
foreign memory so you must always call FREE-STATIC-VECTOR to free it. Use
WITH-STATIC-VECTOR to handle this automatically."
  (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note)
           (optimize speed))
  (check-arguments length element-type initial-element initial-element-p
                   initial-contents initial-contents-p)
  (when alignp
    ;; Check that the alignment is a power of 2 beteeen 16 and 4096.
    #+(and sbcl unix)
    (assert (and (<= +default-alignment+ alignment +max-alignment+)
                 (= 1 (logcount alignment))))
    #-(and sbcl unix)
    (error "Allocation alignment not supported on this implementation."))
  ;; TODO: fix %allocate-static-vector for all implementations
  (let ((vector
          (%allocate-static-vector length element-type
                                   (or alignment +default-alignment+))))
    (if initial-element-p
        (fill vector initial-element)
        (replace vector initial-contents))))

(defmacro with-static-vector ((var length &rest args
                               &key (element-type ''(unsigned-byte 8))
                                 initial-contents initial-element)
                              &body body &environment env)
  "Bind VAR to a static vector of length LENGTH and execute BODY
within its dynamic extent. The vector is freed upon exit."
  (declare (ignorable element-type initial-contents initial-element))
  (multiple-value-bind (real-element-type length type-spec)
      (canonicalize-args env element-type length)
    (let ((args (copy-list args)))
      (remf args :element-type)
      `(sb-sys:without-interrupts
         (let ((,var (make-static-vector ,length ,@args
                                         :element-type ,real-element-type)))
           (declare (type ,type-spec ,var))
           (unwind-protect
                (sb-sys:with-local-interrupts ,@body)
             (when ,var (free-static-vector ,var))))))))

(defmacro with-static-vectors (((var length &rest args) &rest more-clauses)
                               &body body)
  "Allocate multiple static vectors at once."
  `(with-static-vector (,var ,length ,@args)
     ,@(if more-clauses
           `((with-static-vectors ,more-clauses
               ,@body))
           body)))

;;; Static Streams

;; Partially inspired by ELEPHANT's BUFFER-STREAM which provides accessors
;; written in C and provides a Lisp API but not an intermediate Lisp data
;; representation - unlike STATIC-VECTORs. It's worth trying the C-based
;; approach and comparing in the future - I think it's faster but less
;; convenient.

(defvar *default-static-stream-size* 10)
(defclass static-stream (io-stream sb-gray:fundamental-stream)
  ((buffer :initform (make-static-vector *default-static-stream-size*) 
           :initarg :buffer 
           :accessor buffer)
   (offset :initform 0 
           :initarg :offset 
           :accessor offset))
  (:documentation
   "A stream backed by a STATIC-VECTOR."))

(defmethod sb-gray:stream-file-position ((stream static-stream) &optional spec)
  (if spec
      (setf (offset stream) spec)
      (offset spec)))

(defmethod sb-gray:stream-read-byte ((stream static-stream))
  (prog1 (deref (sap-alien (static-vector-pointer (buffer stream)) (* unsigned-char)))
    (incf (offset stream))))

(defmethod sb-gray:stream-read-char ((stream static-stream))
  (prog1 (deref (sap-alien (static-vector-pointer (buffer stream)) (* char)))
    (incf (offset stream))))

(defmethod sb-gray:stream-read-sequence ((stream static-stream) (seq sequence) 
                                         &optional (start 0)
                                                   end)
  (if (= (1- (offset stream)) (length (buffer stream)))
      start
      (let ((inc (if end
                     (- end start)
                     (length seq))))
        (coerce (subseq (buffer stream)
                        (offset stream)
                        inc)
                (type-of seq))
        (incf (offset stream) inc))))

(defmethod sb-gray:stream-write-byte ((stream static-stream) n)
  (prog1 (setf (aref (buffer stream) (offset stream)) n)
    (incf (offset stream))))

(defmethod sb-gray:stream-write-char ((stream static-stream) n)
  (setf (aref (buffer stream) (offset stream)) n))

(defmethod sb-gray:stream-write-sequence ((stream static-stream) (seq sequence)
                                          &optional (start 0)
                                                    (end (length seq)))
  (replace (buffer stream) seq :start1 (offset stream) :start2 start :end2 end))

(defmethod close ((stream static-stream) &key abort)
  (declare (ignore abort))
  (free-static-vector (buffer stream)))

(defmethod sb-gray:stream-write-string ((stream static-stream) string &optional start end)
  (sb-gray:stream-write-sequence stream (sb-ext:string-to-octets string) start end))

(defmethod sb-gray:stream-peek-char ((stream static-stream))
  (aref (buffer stream) (offset stream)))

(defmethod sb-gray:stream-unread-char ((stream static-stream) char)
  ;; we ignore the value and always DECF the offset
  (declare (ignore char))
  (decf (offset stream)))
