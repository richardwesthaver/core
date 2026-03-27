;;; dbus.lisp --- DBUS Codec

;; DBUS Codec Definitions

;;; Code:
(in-package :net/codec/dbus)

;;; Constants
(defconstant +message-no-reply-expected+ 1)
(defconstant +message-no-auto-start+ 2)

;;; Conditions
(define-condition dbus-error (error)
  ())

(define-condition dbus-method-error (dbus-error)
  ((arguments :initarg :arguments))
  (:report (lambda (condition stream)
             (format stream "Method error: ~S."
                     (let ((all-args (slot-value condition 'arguments))
                           (first-arg (first (slot-value condition 'arguments))))
                       (if (stringp first-arg)
                           first-arg
                           all-args))))))

;;; Types
(defclass dbus-type ()
  ((name :initarg :name :reader name)
   (signature :initarg :signature :reader dbus-type-signature)
   (sigexp-formatter :initarg :sigexp-formatter :reader dbus-type-sigexp-formatter)
   (signature-parser :initarg :signature-parser :reader dbus-type-signature-parser)
   (alignment :initarg :alignment :reader dbus-type-alignment)
   (packer :initarg :packer :reader dbus-type-packer)
   (unpacker :initarg :unpacker :reader dbus-type-unpacker)
   (checker :initarg :checker :reader dbus-type-checker)))

(defmethod print-object ((type dbus-type) stream)
  (print-unreadable-object (type stream :type t)
    (format stream "~S" (name type)))
  type)

(defclass dbus-type-table ()
  ((by-name :initform (make-hash-table) :reader dbus-type-table-by-name)
   (by-signature :initform (make-hash-table) :reader dbus-type-table-by-signature)))

(defvar *dbus-type-table*
  (make-instance 'dbus-type-table))

(defun find-dbus-type (designator &optional (table *dbus-type-table*))
  (etypecase designator
    (dbus-type (values designator '()))
    (symbol
     (values
      (or (gethash designator (dbus-type-table-by-name table))
          (error "Can't find DBUS type with name ~S." designator))
      '()))
    (character
     (values
      (or (gethash designator (dbus-type-table-by-signature table))
          (error "Can't find DBUS type with signature ~S." designator))
      '()))
    ((cons symbol)
     (values (find-dbus-type (first designator) table) (rest designator)))))

(defun register-dbus-type (type &optional (table *dbus-type-table*))
  (setf (gethash (name type) (dbus-type-table-by-name table)) type)
  (setf (gethash (dbus-type-signature type) (dbus-type-table-by-signature table)) type)
  table)

(defun parse-signature-from-stream (stream &optional terminator-char num-elements)
  "Parse a signature string from a character stream and return the
corresponding signature expression.

The value of TERMINATOR-CHAR determines when to stop parsing.  If it
is NIL (the default), parsing is stopped when there are no more
characters left to read from the stream.  If it is a character,
parsing is stopped when the same character is read from the stream.

The value of NUM-ELEMENTS determines how many elements (types) should
be read before parsing is stopped.  If it is NIL (the default), there
is no bound on the number of elements to be read."
  (loop for num-read from 0
        for char = (peek-char nil stream nil nil)
        until (or (null char) (eql char terminator-char) (eql num-read num-elements))
        collect (let ((type (find-dbus-type char)))
                  (read-char stream)
                  (funcall (dbus-type-signature-parser type) stream))))

(defun format-sigexp-to-stream (sigexp stream)
  "Format a signature expression as a signature string into a
character stream."
  (dolist (subexp sigexp)
    (multiple-value-bind (type element-types) (find-dbus-type subexp)
      (funcall (dbus-type-sigexp-formatter type) stream element-types))))

(defun valid-signature-p (value element-types)
  "Return true if the value is a valid signature string or signature
expression, and false otherwise."
  (declare (ignore element-types))
  (handler-case
      (progn (signature (sigexp value)) t)
    (error () nil)))

(defun valid-array-p (value element-types)
  "Return true if the value is a sequence with elements of the first
type supplied in ELEMENT-TYPES, and false otherwise."
  (when element-types
    (let ((element-type (first element-types)))
      (and (typep value 'sequence)
           (every (lambda (element) (valid-value-p element element-type))
                  value)))))

(defun valid-struct-p (value element-types)
  "Return true if the value is a sequence with elements matching the
types supplied in ELEMENT-TYPES, and false otherwise."
  (and (typep value 'sequence)
       (= (length value) (length element-types))
       (every (lambda (element element-type) (valid-value-p element element-type))
              value element-types)))

(defun valid-variant-p (value element-types)
  "Return true if the value is a variant value specification, and
false otherwise."
  (declare (ignore element-types))
  (and (listp value)
       (= (length value) 2)
       (valid-value-p (first value) :signature)
       (let ((actual-value (second value))
             (sigexp (sigexp (first value))))
         (valid-value-p actual-value (first sigexp)))))

(defun valid-dict-entry-p (value element-types)
  "Return true if the value is a sequence with two elements, both
matching the types supplied in ELEMENT-TYPES, and false otherwise."
  (and (typep value 'sequence)
       (= (length value) (length element-types) 2)
       (every (lambda (element element-type) (valid-value-p element element-type))
              value element-types)))

(defun valid-value-p (value type)
  "Return true if the value is of the supplied DBUS type, and false
otherwise."
  (multiple-value-bind (type element-types) (find-dbus-type type)
    (funcall (dbus-type-checker type) value element-types)))

(defun make-dbus-type-formatter/parser (name signature composite)
  (etypecase composite
    ((eql nil)
     (values (lambda (stream element-types)
               (declare (ignore element-types))
               (write-char signature stream))
             (lambda (stream)
               (declare (ignore stream))
               name)))
    ((eql t)
     (values (lambda (stream element-types)
               (write-char signature stream)
               (format-sigexp-to-stream element-types stream))
             (lambda (stream)
               (cons name (parse-signature-from-stream stream nil 1)))))
    (character
     (values (lambda (stream element-types)
               (write-char signature stream)
               (format-sigexp-to-stream element-types stream)
               (write-char composite stream))
             (lambda (stream)
               (prog1 (cons name (parse-signature-from-stream stream composite))
                 (read-char stream)))))))

(defmacro define-dbus-type (name &key signature composite alignment pack unpack (checker t))
  (with-gensyms (formatter parser)
    `(progn
       (register-dbus-type
        (multiple-value-bind (,formatter ,parser)
            (make-dbus-type-formatter/parser ',name ',signature ',composite)
          (make-instance 'dbus-type
            :name ',name
            :signature ',signature
            :sigexp-formatter ,formatter
            :signature-parser ,parser
            :alignment ',alignment
            :packer (lambda (stream endianness element-types value)
                      (declare (ignorable element-types value))
                      (with-binary-writers (stream endianness)
                        (align ',alignment)
                        ,pack))
            :unpacker (lambda (stream endianness element-types)
                        (declare (ignorable element-types))
                        (with-binary-readers (stream endianness)
                          (align ',alignment)
                          ,unpack))
            :checker ,(if (and (consp checker) (eq (car checker) 'function))
                          checker
                          `(lambda (value element-types)
                             (declare (ignore element-types))
                             (typep value ',checker))))))
       ',name)))

(defun pack-1 (stream endianness type value)
  "Pack a single DBUS value into stream."
  (multiple-value-bind (type element-types) (find-dbus-type type)
    (funcall (dbus-type-packer type) stream endianness element-types value)))

(defun unpack-1 (stream endianness type)
  "Unpack a single DBUS value from stream."
  (multiple-value-bind (type element-types) (find-dbus-type type)
    (funcall (dbus-type-unpacker type) stream endianness element-types)))

(defun pack-seq (stream endianness types values)
  "Pack a sequence of values into stream."
  (map nil (lambda (type value) (pack-1 stream endianness type value)) types values))

(defun unpack-seq (stream endianness types)
  "Unpack a sequence of DBUS values from stream."
  (map 'list (lambda (type) (unpack-1 stream endianness type)) types))

(defun pack-string (stream endianness value length-size)
  "Pack DBUS string into stream."
  (with-binary-writers (stream endianness)
    (let ((octets (sb-ext:string-to-octets value :external-format :utf-8)))
      (ecase length-size
        (8 (std/io::u8 (length octets)))
        (32 (std/io::u32 (length octets))))
      (map nil #'std/io::u8 octets)
      (std/io::u8 0))))

(defun unpack-string (stream endianness length)
  "Unpack DBUS string from stream."
  (with-binary-readers (stream endianness)
    (prog1 (octets-to-string
            (map-into (make-octets length) #'u8)
            :external-format :utf-8)
      (u8))))

(defun pack-array (stream endianness element-type value)
  "Pack DBUS array into stream."
  (with-binary-writers (stream endianness)
    (let ((length-position (file-position stream)))
      (u32 0)
      (align (alignment element-type))
      (let ((start-position (file-position stream)))
        (pack-seq stream endianness (circular-list element-type) value)
        (let ((end-position (file-position stream)))
          (file-position stream length-position)
          (u32 (- end-position start-position))
          (file-position stream end-position))))))

(defun unpack-array (stream endianness element-type length)
  "Unpack DBUS array from stream."
  (with-binary-readers (stream endianness)
    (align (alignment element-type))
    (loop with start = (stream-read-position stream)
          with end = (+ start length)
          until (= end (stream-read-position stream))
          collect (unpack-1 stream endianness element-type))))

(defun pack-variant (stream endianness element-types value)
  "Pack DBUS variant into stream."
  (pack-1 stream endianness :signature element-types)
  (pack-1 stream endianness (first element-types) value))

(defun unpack-variant (stream endianness)
  "Unpack DBUS variant from stream."
  (with-binary-readers (stream endianness)
    (unpack-1 stream endianness
              (first (sigexp (unpack-string stream endianness (u8)))))))

(defun alignment (type)
  "Return the number of octets to which elements of the supplied type
should be aligned."
  (dbus-type-alignment (find-dbus-type type)))

(defun sigexp (object)
  "Return the signature expression corresponding to the object passed.
If the object is a string, it is assumed to be a signature string,
otherwise it is assumed to be a signature expression and is returned
as-is."
  (if (stringp object)
      (with-input-from-string (in object)
        (parse-signature-from-stream in))
      object))

(defun signature (object)
  "Return the signature string corresponding to the object passed.
If the object is a string, it is assumed to be a signature string and
is returned as-is, otherwise it is assumed to be a signature
expression."
  (if (stringp object)
      object
      (with-output-to-string (out)
        (format-sigexp-to-stream object out))))

(defun pack-value (stream endianness sigexp &rest values)
  "Pack values according to the signature expression and endianness
into stream."
  (pack-seq stream endianness (sigexp sigexp) values))

(defun unpack-value (stream endianness sigexp)
  "Unpack values from stream according to endianness and the signature
expression and return them as a list."
  (unpack-seq stream endianness (sigexp sigexp)))

(defun valid-body-p (body sigexp)
  "Return true if the message body (which is a list of values) is
valid according to the signature expression, and false otherwise."
  (setf sigexp (sigexp sigexp))
  (and (= (length body) (length sigexp))
       (every #'valid-value-p body sigexp)))

;;; Messages
(defclass dbus-message (message) ())

(defclass dbus-standard-message (dbus-message)
  ((endianness :initarg :endianness :reader message-endianness)
   (flags :initarg :flags :reader message-flags)
   (major-protocol-version :initarg :major-protocol-version :reader message-major-protocol-version)
   (body-length :initarg :body-length :reader message-body-length)
   (serial :initarg :serial :reader message-serial)
   (destination :initarg :destination :reader message-destination)
   (sender :initarg :sender :reader message-sender)
   (signature :initarg :signature :reader message-signature)
   (body :initarg :body :reader message-body)))

(defclass dbus-method-call-message (dbus-standard-message)
  ((path :initarg :path :reader path)
   (interface :initarg :interface :reader message-interface)
   (member :initarg :member :reader message-member)))

(defclass dbus-signal-message (dbus-standard-message)
  ((path :initarg :path :reader path)
   (interface :initarg :interface :reader message-interface)
   (member :initarg :member :reader message-member)))

(defclass dbus-method-return-message (dbus-standard-message)
  ((reply-serial :initarg :reply-serial :reader message-reply-serial)))

(defclass dbus-error-message (dbus-standard-message)
  ((error-name :initarg :error-name :reader message-error-name)
   (reply-serial :initarg :reply-serial :reader message-reply-serial)))

(defun encode-dbus-message (endianness type flags major-protocol-version
                            serial path interface member error-name reply-serial
                            destination sender signature body)
  "Encode a DBUS message and return it as an octet vector."
  (io/stream:with-output-to-sequence (out)
    (pack-value out endianness "yyyyuua(yv)"
                (ecase endianness
                  (:little-endian (char-code #\l))
                  (:big-endian (char-code #\B)))
                (ecase type
                  (:method-call 1)
                  (:method-return 2)
                  (:error 3)
                  (:signal 4))
                flags
                major-protocol-version
                0
                serial
                (loop for code from 1
                      for value in (list path interface member error-name
                                         reply-serial destination sender signature)
                      for type across "osssussg"
                      when value collect (list code (list (string type) value))))
    (with-binary-writers (out endianness)
      (std/io::align 8)
      (let ((body-start (file-position out)))
        (apply #'pack-value out endianness (or signature "") body)
        (let ((body-end (file-position out)))
          (file-position out 4)
          (std/io::u32 (- body-end body-start))
          (file-position out body-end))))))

(defun decode-dbus-message (stream)
  "Decode a DBUS message from the stream.

If there are no bytes to be read from the stream, the function
immediately returns NIL.  Otherwise, the function performs blocking
reads until a complete message is decoded. If an end of file occurs,
an error of type END-OF-FILE is signaled.

Unfortunately, due to Common Lisp not having a READ-BYTE-NO-HANG
operator, the stream has to be a bivalent stream."
  (let ((endianness (ecase (when (listen stream) (read-char-no-hang stream))
                      (#\l :little-endian)
                      (#\B :big-endian)
                      ((nil) (return-from decode-dbus-message nil)))))
    (setf (stream-read-position stream) 1)
    (destructuring-bind (type-code flags major-protocol-version
                         body-length serial fields)
        (unpack-value stream endianness "yyyuua(yv)")
      (with-binary-readers (stream endianness)
        (align 8)
        (let (body path interface member error-name
              reply-serial destination sender signature
              unix-fds)
          (declare (ignorable unix-fds))
          (loop for (field-code field-value) in fields
                do (case field-code
                     (1 (setf path field-value))
                     (2 (setf interface field-value))
                     (3 (setf member field-value))
                     (4 (setf error-name field-value))
                     (5 (setf reply-serial field-value))
                     (6 (setf destination field-value))
                     (7 (setf sender field-value))
                     (8 (setf signature field-value))
                     (9 (setf unix-fds field-value))
                     (t (warn "Unknown field code ~D; ignoring field." field-code))))
          (setf body (unpack-value stream endianness signature))
          (macrolet ((make-message (class-name &rest additional-initargs)
                       `(make-instance ,class-name
                          :endianness endianness
                          :flags flags
                          :major-protocol-version major-protocol-version
                          :body-length body-length
                          :serial serial
                          :destination destination
                          :sender sender
                          :signature signature
                          :body body
                          ,@additional-initargs)))
            (case type-code
              (1 (make-message 'dbus-method-call-message :path path :interface interface :member member))
              (2 (make-message 'dbus-method-return-message :reply-serial reply-serial))
              (3 (make-message 'dbus-error-message :error-name error-name :reply-serial reply-serial))
              (4 (make-message 'dbus-signal-message :path path :interface interface :member member))
              (t (warn "Unknown message type code ~D; ignoring message." type-code)))))))))

(defun invoke-method (connection member
                      &key path signature arguments interface destination
                           no-reply no-auto-start asynchronous (endianness :little-endian))
  (let ((serial (next-id connection)))
    (send-message
     (encode-dbus-message endianness :method-call
                          (logior (if no-reply +message-no-reply-expected+ 0)
                                  (if no-auto-start +message-no-auto-start+ 0))
                          1 serial path interface member nil nil
                          destination nil signature arguments)
     connection)
    (if (or no-reply asynchronous)
        serial
        (multiple-value-bind (body message)
            (wait-for-reply serial connection)
          (etypecase message
            (dbus-method-return-message (values-list body))
            (dbus-error-message (error 'dbus-method-error :arguments body)))))))

(defmethod deserialize ((self stream) (fmt (eql :dbus)) &key)
  (decode-dbus-message self))

;;; Type Defs
(define-dbus-type :byte
  :signature #\y
  :alignment 1
  :pack (u8 value)
  :unpack (u8)
  :checker (unsigned-byte 8))

(define-dbus-type :boolean
  :signature #\b
  :alignment 4
  :pack (u32 (if value 1 0))
  :unpack (if (zerop (u32)) nil t))

(define-dbus-type :int16
  :signature #\n
  :alignment 2
  :pack (u16 (signed-to-unsigned value 16))
  :unpack (unsigned-to-signed (u16) 16)
  :checker (signed-byte 16))

(define-dbus-type :uint16
  :signature #\q
  :alignment 2
  :pack (u16 value)
  :unpack (u16)
  :checker (unsigned-byte 16))

(define-dbus-type :int32
  :signature #\i
  :alignment 4
  :pack (u32 (signed-to-unsigned value 32))
  :unpack (unsigned-to-signed (u32) 32)
  :checker (signed-byte 32))

(define-dbus-type :uint32
  :signature #\u
  :alignment 4
  :pack (u32 value)
  :unpack (u32)
  :checker (unsigned-byte 32))

(define-dbus-type :int64
  :signature #\x
  :alignment 8
  :pack (u64 (signed-to-unsigned value 64))
  :unpack (unsigned-to-signed (u64) 64)
  :checker (signed-byte 64))

(define-dbus-type :uint64
  :signature #\t
  :alignment 8
  :pack (u64 value)
  :unpack (u64)
  :checker (unsigned-byte 64))

(define-dbus-type :double
  :signature #\d
  :alignment 8
  :pack (u64 (encode-float64 (float value 0.0d0)))
  :unpack (decode-float64 (u64))
  :checker real)

(define-dbus-type :string
  :signature #\s
  :alignment 4
  :pack (pack-string stream endianness value 32)
  :unpack (unpack-string stream endianness (u32))
  :checker string)

(define-dbus-type :object-path
  :signature #\o
  :alignment 4
  :pack (pack-string stream endianness value 32)
  :unpack (unpack-string stream endianness (u32))
  :checker string)

(define-dbus-type :signature
  :signature #\g
  :alignment 1
  :pack (pack-string stream endianness (signature value) 8)
  :unpack (unpack-string stream endianness (u8))
  :checker #'valid-signature-p)

(define-dbus-type :array
  :signature #\a
  :composite t
  :alignment 4
  :pack (pack-array stream endianness (first element-types) value)
  :unpack (unpack-array stream endianness (first element-types) (u32))
  :checker #'valid-array-p)

(define-dbus-type :struct
  :signature #\(
  :composite #\)
  :alignment 8
  :pack (pack-seq stream endianness element-types value)
  :unpack (unpack-seq stream endianness element-types)
  :checker #'valid-struct-p)

(define-dbus-type :variant
  :signature #\v
  :alignment 1
  :pack (pack-variant stream endianness (sigexp (first value)) (second value))
  :unpack (unpack-variant stream endianness)
  :checker #'valid-variant-p)

(define-dbus-type :dict-entry
  :signature #\{
  :composite #\}
  :alignment 8
  :pack (pack-seq stream endianness element-types value)
  :unpack (unpack-seq stream endianness element-types)
  :checker #'valid-dict-entry-p)

(define-dbus-type :unix-fd
  :signature #\h
  :alignment 4
  :pack (u32 value)
  :unpack (u32)
  :checker (unsigned-byte 32))

;;; Objects
(defvar *all-dbus-objects* '())

(defclass child-object-mixin ()
  ((child-object-names :initarg :child-object-names :initform '()
                       :accessor dbus-object-child-object-names)
   (parent-object-name :initarg :parent-object-name
                       :accessor dbus-object-parent-object-name)))

(defmethod register-child-object ((child-object child-object-mixin)
                                  (parent-object child-object-mixin))
  (pushnew (name child-object) (dbus-object-child-object-names parent-object))
  (setf (dbus-object-parent-object-name child-object) (name parent-object)))

(defclass introspection-mixin () ())

(defclass dbus-object (introspection-mixin child-object-mixin)
  ((name :initarg :name :reader name)
   (path :initarg :path :accessor path)
   (method-handlers :initform (make-hash-table :test 'equal) :reader dbus-object-method-handlers)
   (signal-handlers :initform (make-hash-table :test 'equal) :reader dbus-object-signal-handlers)))

(defgeneric dbus-object-handler-lookup-table (message object))

(defmethod dbus-object-handler-lookup-table ((message dbus-signal-message) (object dbus-object))
  (dbus-object-signal-handlers object))

(defmethod dbus-object-handler-lookup-table ((message dbus-method-call-message) (object dbus-object))
  (dbus-object-method-handlers object))

(defun find-dbus-object (name)
  (check-type name symbol)
  (get name 'dbus-object))

(defun (setf find-dbus-object) (new-value name)
  (check-type new-value (or null dbus-object))
  (cond ((null new-value)
         (setf *all-dbus-objects* (remove name *all-dbus-objects*))
         (remprop name 'dbus-object)
         nil)
        (t
         (pushnew name *all-dbus-objects*)
         (setf (get name 'dbus-object) new-value))))

(defun register-dbus-object (name path &optional dbus-object-sub-class)
  (check-type name symbol)
  (check-type path string)
  (if (find-dbus-object name)
      ;; If we already have an object with that name, just update its
      ;; path.
      (setf (path (find-dbus-object name)) path)
      (if dbus-object-sub-class
          (setf (find-dbus-object name)
                (make-instance dbus-object-sub-class
                  :name name
                  :path path))
          (setf (find-dbus-object name)
                (make-instance 'dbus-object
                  :name name
                  :path path))))
  name)

(defun require-dbus-object (name)
  (loop with object = (find-dbus-object name)
        while (not (typep object 'dbus-object))
        do (setf object (missing-entry name :error))
           ;; We can also accept a new object name.
           (when (symbolp object)
             (shiftf name object (find-dbus-object object)))
        finally (return (values object (name object)))))

(defmacro initialize-mixined-instance (name &body options)
  (let ((parent nil) (class 'dbus-object))
    (dolist (option options)
      (when (and (consp option) (eq (car option) :parent))
        (setf parent (cadr option)))
      (when (and (consp option) (eq (car option) :class))
        (setf class (cadr option))))
    `(progn
       (if ',parent
           (register-child-object (find-dbus-object ',name)
                                  (find-dbus-object ',parent)))
       (if (subtypep ',class 'introspection-mixin)
           (define-dbus-method (,name introspect) () (:string)
             (:interface "org.freedesktop.DBus.Introspectable")
             (introspection-document (find-dbus-object ',name)))))))

(defmacro define-dbus-object (name &body options)
  (let ((path nil) (class 'dbus-object))
    (dolist (option options)
      (when (and (consp option) (eq (car option) :path))
        (setf path (cadr option)))
      (when (and (consp option) (eq (car option) :class))
        (setf class (cadr option))))
    `(prog1
         (register-dbus-object ',name ,path ',class)
       (initialize-mixined-instance ,name ,@options))))

;;; Define handlers
(defclass handler ()
  ((object-name :initarg :object-name :reader handler-object-name)
   (lisp-name :initarg :lisp-name :reader handler-lisp-name)
   (name :initarg :name :reader name)
   (interface :initarg :interface :reader handler-interface)
   (input-signature :initarg :input-signature :reader handler-input-signature)
   (function :initarg :function :reader handler-function)))

(defgeneric handler-full-lisp-name (handler))

(defmethod handler-full-lisp-name ((handler handler))
  (list (handler-object-name handler) (handler-lisp-name handler)))

(defun full-member-name (interface member)
  (concatenate 'string interface "." member))

(defun stringify-lisp-name (lisp-name)
  "Return a string that is the capitalized symbol name of LISP-NAME,
sans dashes."
  (remove #\- (string-capitalize lisp-name)))

(defclass dbus-method-handler (handler)
  ((output-signature :initarg :output-signature :reader handler-output-signature)))

(defun register-dbus-method (object-name method-name name-string interface parameter-types return-types handler)
  (check-type method-name symbol)
  (check-type name-string string)
  (check-type interface string)
  (multiple-value-bind (object object-name)
      (require-dbus-object object-name)
    (setf (gethash (full-member-name interface name-string)
                   (dbus-object-method-handlers object))
          (make-instance 'dbus-method-handler
            :object-name object-name
            :lisp-name method-name
            :name name-string
            :interface interface
            :input-signature parameter-types
            :output-signature return-types
            :function handler))
    (list object-name method-name)))

(defmacro define-dbus-method ((object-name method-name) (&rest parameters) (&rest return-types) &body body)
  (let ((name-string (stringify-lisp-name method-name))
        (interface nil)
        (parameter-names (mapcar #'first parameters))
        (parameter-types (mapcar #'second parameters)))
    ;; Set options.
    (loop while (and (consp (car body)) (keywordp (caar body)))
          do (let ((option (pop body)))
               (ecase (car option)
                 (:name
                  (setf name-string (cadr option)))
                 (:interface
                  (setf interface (cadr option))))))
    ;; Register the method with the object.
    `(register-dbus-method ',object-name
                           ',method-name
                           ,name-string
                           ,interface
                           ',parameter-types
                           ',return-types
                           (lambda (,@parameter-names)
                             ,@body))))

(defclass dbus-signal-handler (handler)
  ())

(defun register-dbus-signal-handler (object-name handler-name name-string interface parameter-types handler)
  (check-type handler-name symbol)
  (check-type name-string string)
  (check-type interface string)
  (multiple-value-bind (object object-name)
      (require-dbus-object object-name)
    (setf (gethash (full-member-name interface name-string)
                   (dbus-object-signal-handlers object))
          (make-instance 'dbus-signal-handler
            :object-name object-name
            :lisp-name handler-name
            :name name-string
            :interface interface
            :input-signature parameter-types
            :function handler))
    (list object-name handler-name)))

(defmacro define-dbus-signal-handler ((object-name handler-name) (&rest parameters) &body body)
  (let ((name-string (stringify-lisp-name handler-name))
        (interface nil)
        (parameter-names (mapcar #'first parameters))
        (parameter-types (mapcar #'second parameters)))
    ;; Set options.
    (loop while (and (consp (car body)) (keywordp (caar body)))
          do (let ((option (pop body)))
               (ecase (car option)
                 (:name
                  (setf name-string (cadr option)))
                 (:interface
                  (setf interface (cadr option))))))
    ;; Register the signal handler with the object.
    `(register-dbus-signal-handler ',object-name
                                   ',handler-name
                                   ,name-string
                                   ,interface
                                   ',parameter-types
                                   (lambda (,@parameter-names)
                                     ,@body))))

;;; introspection functions
(defgeneric output-introspection-fragment (thing)
  (:documentation "Return the introspection element for a thing."))

(defmethod relative-path-string ((object child-object-mixin))
  (let* ((object-path (path object))
         (parent-object-path
           (path
            (find-dbus-object (dbus-object-parent-object-name object))))
         (parent-object-directory
           (if (string= "/" parent-object-path)
               parent-object-path
               (concatenate 'string parent-object-path "/")))
         (len (length parent-object-directory)))
    (if (string= parent-object-directory (subseq object-path 0 len))
        (subseq object-path len)
        (error (format nil "\"~a\" isn't a child object path of \"~a\""
                       object-path parent-object-path)))))

(defmethod output-introspection-fragment ((thing child-object-mixin))
  (make-xml-node :name "node" :attrs `(("name" . ,(relative-path-string thing)))))

(defmethod output-introspection-fragment ((thing dbus-method-handler))
  (make-xml-node 
   :name "method"
   :attrs `(("name" ,(name thing)))
   :children
   (flet
       ((one-arg (name dir type)
          (make-xml-node 
           :name "arg"
           :attrs `(("direction" . ,dir)
                    ("type" . ,(signature (list type)))
                    . ,(when name
                         `(("name" . ,(stringify-lisp-name name))))))))
     (loop for type in (handler-input-signature thing)
           do (one-arg nil "in" type))
     (loop for type in (handler-output-signature thing)
           do (one-arg nil "out" type)))))

(defmethod output-introspection-fragment ((thing dbus-signal-handler))
  (make-xml-node 
   :name "signal" :attrs `(("name" . ,(name thing)))
   :children (flet ((one-arg (name type)
                      (make-xml-node :name "arg"
                                     :attrs `(("type" . ,(signature (list type)))
                                              . (when name `(("name" . ,,(stringify-lisp-name name))))))))
               (loop for type in (handler-input-signature thing)
                     do (one-arg nil type)))))

(defmethod collect-handlers-by-interface ((object dbus-object))
  (let ((result (make-hash-table :test #'equal)))
    (loop for m-h being the hash-values of (dbus-object-method-handlers object)
          do (push m-h (gethash (handler-interface m-h) result ())))
    (loop for s-h being the hash-values of (dbus-object-signal-handlers object)
          do (push s-h (gethash (handler-interface s-h) result ())))
    result))

(defgeneric introspection-document (object)
  (:documentation "Return the introspection document string for
a particular DBUS  object."))

(defmethod introspection-document ((object child-object-mixin))
  (with-output-to-string (s)
    (dat/xml::write-doctype
     "node" 
     '(PUBLIC "-//freedesktop//DTD D-BUS Object Introspection 1.0//EN" 
       "http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd")
     s)
    (write-xml
     (make-xml-node 
      :name "node"
      :children
      (let ((interfaces-handlers (collect-handlers-by-interface object))
            (child-object-names (dbus-object-child-object-names object)))
        (loop for interface-name being the hash-keys of interfaces-handlers
              using (hash-value handlers)
              do (make-xml-node :name "interface"
                                :attrs `(("name" . ,interface-name))
                                :children (loop for h in handlers
                                                do (output-introspection-fragment h))))
        (dolist (child-object-name child-object-names)
          (output-introspection-fragment (find-dbus-object child-object-name)))))
     s)))
