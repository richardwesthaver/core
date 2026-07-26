;;; std/io.lisp --- Lisp Serialization and IO Tables

;; Read/Write Lisp Objects. Binary object de/serialization.

;;; Commentary:

;; This package contains macros for defining a pair of functions for a
;; category of lisp types - a READ-* function and a WRITE-* function. These
;; functions operate on a storage context which we Serialize (write) and Deserialize
;; (read) values from.

;; Within the STD system we implement the API for octet vectors as well as
;; (ALIEN (* UNSIGNED-CHAR)). These are used by higher-level packages which
;; need to portably serialize lisp objects as octet vectors.

;;; Code:
(in-package :std/io)

(eval-always
  (define-condition serde-condition () ()
    (:documentation "Default SERDE condition class."))
  (deferror serde-error (serde-condition) ()
    (:documentation "An error signaled during serialization OR deserialization.")))

(deferror serializer-error (serde-error)
  ()
  (:documentation "An error which occurs during object serialization."))

(deferror deserializer-error (serde-error) 
  ()
  (:documentation "An error which occurs during object deserialization."))

(defgeneric serialize (obj format &key &allow-other-keys)
  (:documentation "Serialize OBJ to FORMAT."))

(defgeneric deserialize (from format &key &allow-other-keys)
  (:documentation "Deserialize FROM into an object of type FORMAT."))

(defgeneric ser (self)
  (:documentation "Access the serializer of SELF."))

(defgeneric (setf ser) (new self))

(defgeneric de (self)
  (:documentation "Access the deserializer of SELF."))

(defgeneric (setf de) (new self))

(defgeneric serde (from to)
  (:documentation "Point-to-point serialization.

FROM and TO should both specialize on object instances.

Calling this function requires you to initialize the arguments instead of
relying on a designated format and generating an object in the method body."))

;; (defmacro defde (fmt &body body))
;; (defmacro defser (fmt &body body))
;; (defmacro defserde (fmt &body body))

(defglobal *io-table* (make-hash-table))

(defmacro define-io (name &body body)
  "Define a set of readers and writers of category NAME.

BODY contains elements of the form:

(OBJECT (:READ (ARGS) BODY) (:WRITE (ARGS) BODY))

and generates functions of the form READ/WRITE-NAME?-OBJECT

OBJECT may also be a cons in which case the car is an alias for the actual
type in the cdr."
  ;; reset io-table entry
  (when body
    (with-gensyms (readers writers)
        `(progn
           (defmacro ,(symbolicate 'read- name) (ty from)
             `(,(intern (string (symbolicate 'read- ',name '- ty)) ,*package*) ,from))
           (defmacro ,(symbolicate 'write- name) (ty obj to)
             `(,(intern (string (symbolicate 'write- ',name '- ty)) ,*package*) ,to ,obj))
           (let* ((,readers)
                  (,writers))
             ,@(loop for form in body
                     append 
                        (let* ((type (car form))
                               (type-name (if (consp type)
                                              (if #1=(and (oddp (length type)) (getf (cdr type) :alias))
                                                  #1#
                                                  (format nil "~@[~{~A-~^~A~}~]" type))
                                              type))
                               (rfn (symbolicate 'read- name '- type-name))
                               (wfn (symbolicate 'write- name '- type-name)))
                          `(,@(when-let ((rf (cdr (assoc :read (cdr form)))))
                                (when #2=(cdr rf)
                                      `((push (defun ,rfn ,(car rf)
                                                ,@(if (atom #2#) (list #2#) #2#)) 
                                              ,readers))))
                            ,@(when-let ((wf (cdr (assoc :write (cdr form)))))
                                (when #3=(cdr wf)
                                      `((push 
                                         (defun ,wfn ,(car wf) ,@(if (atom #3#) (list #3#) #3#)) 
                                         ,writers)))))))
             (setf (gethash ,name *io-table*) (list :read ,readers :write ,writers)))))))

;;; Binary Stream IO
(defvar *stream-read-positions*
  (make-hash-table :weakness :key)
  "A mapping from a stream (weakly referenced) to a read position.")

(defun stream-read-position (stream)
  "Return the stream's read position (zero by default)."
  (gethash stream *stream-read-positions* 0))

(defun (setf stream-read-position) (new-read-position stream)
  "Set the stream's read position to a new value."
  (setf (gethash stream *stream-read-positions*) new-read-position))

(defmacro with-binary-writers ((stream endianness) &body forms)
  "Evaluate forms with functions to write binary data to the stream in
a given endianness.

  STREAM

    A form evaluating to a binary output stream with a file position.

  ENDIANNESS

    A form evaluating to either :LITTLE-ENDIAN or :BIG-ENDIAN.

Local functions:

  ALIGN

    A function that takes an integer and ensures the stream's file
    position is aligned to it.  It does so by writing the appropriate
    number of 0 octets.

  U8, U16, U32, U64

    Functions that take 8-, 16-, 32-, and 64-bit unsigned byte values,
    respectively, and write these values to the stream, in the
    appropriate endianness.  The values are always naturally aligned
    before written."
  (once-only (stream)
    (with-gensyms (body-function-name u8-var u16-var u32-var u64-var)
      `(flet ((,body-function-name (,u8-var ,u16-var ,u32-var ,u64-var)
                (labels ((align (n)
                           (loop until (zerop (mod (file-position ,stream) n)) do (u8 0)))
                         (u8 (value)
                           (funcall ,u8-var value))
                         (u16 (value)
                           (align 2)
                           (funcall ,u16-var value))
                         (u32 (value)
                           (align 4)
                           (funcall ,u32-var value))
                         (u64 (value)
                           (align 8)
                           (funcall ,u64-var value)))
                  (declare (inline align u8 u16 u32 u64))
                  (declare (ignorable #'align #'u8 #'u16 #'u32 #'u64))
                    ,@forms)))
           (ecase ,endianness
             (:little-endian
              (macrolet ((u (size)
                           `(lambda (value)
                              ,@(loop for i from 0 below size by 8
                                      collect `(write-byte (ldb (byte 8 ,i) value) ,',stream)))))
                (,body-function-name (u 8) (u 16) (u 32) (u 64))))
             (:big-endian
              (macrolet ((u (size)
                           `(lambda (value)
                              ,@(loop for i from (- size 8) downto 0 by 8
                                      collect `(write-byte (ldb (byte 8 ,i) value) ,',stream)))))
                (,body-function-name (u 8) (u 16) (u 32) (u 64)))))))))

(defmacro with-binary-readers ((stream endianness) &body forms)
  "Evaluate forms with functions to read binary data from the stream
in a given endianness.

  STREAM

    A form evaluating to a binary input stream.

  ENDIANNESS

    A form evaluating to either :LITTLE-ENDIAN or :BIG-ENDIAN.

Local functions:

  ALIGN

    A function that takes an integer and ensures the stream's read
    position is aligned to it.  It does so by reading and ignoring the
    appropriate number of octets.

  U8, U16, U32, U64

    Functions that read 8-, 16-, 32-, and 64-bit unsigned byte values,
    respectively, from the stream, in the appropriate endianness.  The
    read position is ensured to be naturally aligned before reading
    the value."
  (once-only (stream)
      (with-gensyms (body-function-name u8-var u16-var u32-var u64-var)
        `(flet ((,body-function-name (,u8-var ,u16-var ,u32-var ,u64-var)
                  (labels ((align (n)
                             (loop until (zerop (mod (stream-read-position ,stream) n)) do (u8)))
                           (u8 ()
                             (funcall ,u8-var))
                           (u16 ()
                             (align 2)
                             (funcall ,u16-var))
                           (u32 ()
                             (align 4)
                             (funcall ,u32-var))
                           (u64 ()
                             (align 8)
                             (funcall ,u64-var)))
                    (declare (inline align u8 u16 u32 u64))
                    (declare (ignorable #'align #'u8 #'u16 #'u32 #'u64))
                    ,@forms)))
           (ecase ,endianness
             (:little-endian
              (macrolet ((u (size)
                           `(lambda ()
                              (let ((value 0))
                                ,@(loop for i from 0 below size by 8
                                        collect `(setf (ldb (byte 8 ,i) value)
                                                       (read-byte ,',stream)))
                                (incf (stream-read-position ,',stream) ,(floor size 8))
                                value))))
                (,body-function-name (u 8) (u 16) (u 32) (u 64))))
             (:big-endian
              (macrolet ((u (size)
                           `(lambda ()
                              (let ((value 0))
                                ,@(loop for i from (- size 8) downto 0 by 8
                                        collect `(setf (ldb (byte 8 ,i) value)
                                                       (read-byte ,',stream)))
                                (incf (stream-read-position ,',stream) ,(floor size 8))
                                value))))
                (,body-function-name (u 8) (u 16) (u 32) (u 64)))))))))
