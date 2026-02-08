;;; tar.lisp --- Tarballs

;; Unix Tape Archive Formats.

;;; Commentary:

;; wiki: https://en.wikipedia.org/wiki/Tar_(computing)
;; gnu-tar: https://www.gnu.org/software/tar/manual/html_node/Standard.html

;; ustar: https://wiki.osdev.org/USTAR

;; USTAR is the widely-available POSIX standard.

;; impl: https://github.com/froydnj/archive
;; impl: https://gitlab.common-lisp.net/cl-tar

;; rust impl: https://github.com/alexcrichton/tar-rs

;;; Code:
(in-package :dat/tar)

;;; Vars
(defvar *tar-block-bytes* 512)

(defvar *tar-record-blocks* 20)

(defvar *tar-record-bytes* (* *tar-block-bytes* *tar-record-blocks*))

;;; values for tar's `typeflag' field
(defconstant +tar-regular-file+ #x30)
;;; backwards compatibility
(defconstant +tar-regular-alternate-file+ #x00)
(defconstant +tar-hard-link+ #x31)
(defconstant +tar-symbolic-link+ #x32)
(defconstant +tar-character-device+ #x33)
(defconstant +tar-block-device+ #x34)
(defconstant +tar-directory-file+ #x35)
(defconstant +tar-fifo-device+ #x36)
(defconstant +tar-implementation-specific-file+ #x37)

(defconstant +posix-extended-header+ #x78)
(defconstant +posix-global-header+ #x67)

;;; non-standard typeflags
(defconstant +gnutar-long-link-name+ #x4b)
(defconstant +gnutar-long-name+ #x4c)
(defconstant +gnutar-sparse+ #x53)
(defconstant +gnutar-directory-dump+ #x44)
(defconstant +gnutar-volume-header-name+ #x56)

(defconstant +ascii-space+ #x20)
(defconstant +ascii-zero+ #x30)
(defconstant +ascii-nine+ #x39)
(defconstant +ascii-a+ #x61)
(defconstant +ascii-z+ #x7a)
(defconstant +ascii-/+ #x29)
(defconstant +ascii-newline+ #xa)

;;; Conditions
(define-condition tar-error () ())
(deferror simple-tar-error (tar-error simple-error) () (:auto t))
(deferror invalid-checksum-error (tar-error)
    ((provided :initarg :provided :reader error-provided)
     (computed :initarg :computed :reader error-computed))
    (:report (lambda (condition stream)
               (format stream "Invalid tar header checksum ~D (wanted ~D)"
                       (error-provided condition) (error-computed condition))))
    (:documentation "Signaled when the checksum in a tar header is invalid."))

(define-condition malformed-pax-attribute-entry (tar-error) ())

;;; Macros
(eval-always
  (defun round-up-to-tar-block (num)
    (* (ceiling num *tar-block-bytes*) *tar-block-bytes*))

  (defun tar-checksum-guts (header-type block start transform-fun)
    (declare (type (simple-array (unsigned-byte 8) (*)) block))
    (let* ((end (+ start *tar-block-bytes*))
           (checksum-offset (field-offset header-type 'checksum))
           (checksum-start (+ start checksum-offset))
           (checksum-end (+ start checksum-offset
                            (field-length header-type 'checksum))))
      (loop for i from start below end
            sum (if (or (< i checksum-start) (<= checksum-end i))
                    (funcall transform-fun (aref block i))
                    +ascii-space+))))

  (defun compute-checksum-for-tar-header (header-type block start)
    (tar-checksum-guts header-type block start #'identity))

  (defun compute-old-checksum-for-tar-header (header-type block start)
    (tar-checksum-guts header-type block start #'(lambda (b) (if (< b 128) b (- b 256)))))

  (defun tar-block-checksum-matches-p (header-type block checksum start)
    (let ((sum (compute-checksum-for-tar-header header-type block start)))
      (if (= sum checksum)
          t
          ;; try the older, signed arithmetic way
          (let ((signed-sum (compute-old-checksum-for-tar-header header-type block start)))
            (values (= signed-sum checksum) sum)))))

  (defun null-block-p (block start)
    (declare (type (simple-array (unsigned-byte 8) (*)) block))
    (null (position-if-not #'zerop block
                           :start start :end (+ start *tar-block-bytes*))))

  (defun extractor-function-name (entry-name field-name)
    (intern (with-standard-io-syntax (format nil "~A-READ-~A-FROM-BUFFER" entry-name field-name))))

  (defun injector-function-name (entry-name field-name)
    (intern (with-standard-io-syntax (format nil "~A-WRITE-~A-TO-BUFFER" entry-name field-name)))))

(defgeneric field-offset (header field-name))

(defgeneric field-length (header field-name))

(defmacro define-octet-header (class-name &rest field-defs)
  (let ((offset 0))                     ; could be integrated in the LOOP?
    (flet ((keywordify-name (name)
             (intern (symbol-name name) (find-package "KEYWORD"))))
      (loop for (name length kind constant) in field-defs
            collect `(defmethod field-offset ((header ,class-name) (field-name (eql ',name)))
                       ,offset) into offset-defs
            collect `(defmethod field-offset ((header (eql ',class-name)) (field-name (eql ',name)))
                       ,offset) into offset-defs
            collect `(defmethod field-length ((header ,class-name) (field-name (eql ',name)))
                       ,length) into length-defs
            collect `(defmethod field-length ((header (eql ',class-name)) (field-name (eql ',name)))
                       ,length) into length-defs
            collect `(defun ,(extractor-function-name class-name name) (buffer entry-start encoding)
                       (declare (type (simple-array (unsigned-byte 8) (*)) buffer))
                       (declare (ignorable encoding))
                       ,(ecase kind
                          (:string
                           `(octets-to-string
                             (read-octets-from-buffer buffer :start (+ entry-start ,offset)
                                                              :end (+ entry-start ,offset ,length) :nullp nil)
                             :external-format encoding))
                          (:string-null
                           `(octets-to-string
                             (read-octets-from-buffer buffer :start (+ entry-start ,offset)
                                                             :end (+ entry-start ,offset ,length) :nullp t)
                             :external-format encoding))
                          (:byte
                           (unless (= length 1)
                             (error ":BYTE fields cannot be longer than 1"))
                           `(aref buffer (+ entry-start ,offset)))
                          (:bytes
                           `(subseq buffer (+ entry-start ,offset) (+ entry-start ,offset ,length)))
                          (:octnum `(read-number-from-buffer buffer :start (+ entry-start ,offset)
                                                                    :end (+ entry-start ,offset ,length) :radix 8))
                          (:hexnum `(read-number-from-buffer buffer :start (+ entry-start ,offset)
                                                                    :end (+ entry-start ,offset ,length) :radix 16)))) into reader-defs
            collect `(defun ,(injector-function-name class-name name) (buffer entry-start thing encoding)
                       (declare (type (simple-array (unsigned-byte 8) (*)) buffer))
                       (declare (ignorable encoding))
                       ,(ecase kind
                          ((:string :string-null)
                           `(let ((thing (string-to-octets thing :external-format encoding)))
                              (dotimes (i (length thing) (values))
                                (setf (aref buffer (+ entry-start ,offset i)) (aref thing i)))))
                          (:byte
                           `(setf (aref buffer (+ entry-start ,offset)) thing))
                          (:bytes
                           `(setf (subseq buffer (+ entry-start ,offset) (+ entry-start ,offset ,length))
                                  thing))
                          (:octnum
                           `(let ((start (+ entry-start ,offset))
                                  (end (+ entry-start ,offset ,length)))
                              (write-number-to-buffer thing buffer :start start :end end :radix 8 :nullp t)))
                          (:hexnum
                           `(let ((start (+ entry-start ,offset))
                                  (end (+ entry-start ,offset ,length)))
                              (write-number-to-buffer thing buffer :start start :end end :radix 16 :nullp nil))))
                       (values)) into writer-defs
            collect `(,name :initarg ,(keywordify-name name)
                            :accessor ,name) into slot-definitions
            append `(,(keywordify-name name)
                     ,(if constant
                          constant
                          (case kind
                            ((:string :string-null) "")
                            (t 0)))) into default-initargs
            do (incf offset length)
            finally (return
                      `(progn
                         (defclass ,class-name ()
                           ,slot-definitions
                           (:default-initargs ,@default-initargs))
                         ,@length-defs
                         ,@offset-defs
                         ,@reader-defs
                         ,@writer-defs
                         (defmethod header-length ((header ,class-name))
                           ,offset)
                         (defmethod header-length ((header (eql ',class-name)))
                           ,offset)
                         (defmethod write-header-to-buffer ((header ,class-name) buffer encoding &optional (start 0))
                           (declare (type (simple-array (unsigned-byte 8) (*)) buffer))

                           ;; Ensure we can write the entire header to this
                           ;; buffer.
                           (assert (<= (+ start *tar-block-bytes*) (length buffer)))
                           ;; Ensure a clean slate
                           (fill buffer 0 :start start :end (+ start *tar-block-bytes*))

                           ,@(loop
                               :for (name length kind) :in field-defs
                               :unless (eql name 'checksum)
                                 :collect `(,(injector-function-name class-name name) buffer start (,name header)
                                            encoding))

                           ;; Write the checksum
                           (let* ((checksum (compute-checksum-for-tar-header header buffer start))
                                  (checksum-offset (+ start (field-offset header 'checksum))))
                             (write-number-to-buffer checksum buffer
                                                     :start checksum-offset
                                                     :end (+ checksum-offset
                                                             (field-length header 'checksum)
                                                             -2)
                                                     :radix 8)
                             ;; terminated with a NULL and then a space (!?)
                             (setf (aref buffer (+ checksum-offset 6)) 0
                                   (aref buffer (+ checksum-offset 7)) +ascii-space+)))
                         (defmethod read-header-from-buffer ((header (eql ',class-name)) buffer encoding &key (start 0))
                           (let ((checksum (,(extractor-function-name class-name 'checksum) buffer start encoding)))
                             (multiple-value-bind (validp computed)
                                 (tar-block-checksum-matches-p header buffer checksum start)
                               (unless validp
                                 (error 'invalid-checksum-error
                                        :provided checksum :computed computed))
                               (make-instance header
                                              ,@(loop
                                                  :for (name length kind) :in field-defs
                                                  :unless (eql name '%%padding)
                                                    :appending `(,(keywordify-name name)
                                                                 (,(extractor-function-name class-name name)
                                                                  buffer start encoding)))))))))))))

;;; Protocol
(defgeneric close-tar-file (tar-file)
  (:documentation
   "Closes the stream associated with TAR-FILE and the tar-file itself.
Further operations on the tar-file are undefined.

Does NOT close the underlying STREAM that backed the TAR-FILE."))

(defgeneric mode (entry)
  (:documentation "Return the mode of the ENTRY (an integer)."))

(defgeneric uid (entry)
  (:documentation "Return the uid of the ENTRY (an integer)."))

(defgeneric gid (entry)
  (:documentation "Return the gid of the ENTRY (an integer)."))

;; (defgeneric size (entry)
;;   (:documentation "Return the size of the ENTRY (an integer)."))

(defgeneric mtime (entry)
  (:documentation "Return the mtime of the ENTRY (an integer)."))

(defgeneric linkname (entry)
  (:documentation "Return the linkname of the ENTRY (a string)."))

(defgeneric uname (entry)
  (:documentation "Return the uname of the ENTRY (a string)."))

(defgeneric gname (entry)
  (:documentation "Return the gname  of the ENTRY (a string)."))

(defgeneric devmajor (entry)
  (:documentation "Return the major device of the ENTRY (an integer)."))

(defgeneric devminor (entry)
  (:documentation "Return the minor device of the ENTRY (an integer)."))

(defgeneric prefix (entry)
  (:documentation "Return the prefix of the ENTRY (a string)."))

(defgeneric atime (entry)
  (:documentation "Return the atime of the ENTRY (an integer)."))

(defgeneric ctime (entry)
  (:documentation "Return the ctime of the ENTRY (an integer)."))

;; (defgeneric offset (entry)
;;   (:documentation "Return the offset of the ENTRY (an integer)."))

(defgeneric offset-sparse-0 (entry)
  (:documentation "Return the offset of the first sparse block of the ENTRY (an integer)."))

(defgeneric numbytes-sparse-0 (entry)
  (:documentation "Return the numbytes of the first sparse block of the ENTRY (an integer)."))

(defgeneric offset-sparse-1 (entry)
  (:documentation "Return the offset of the second sparse block of the ENTRY (an integer)."))

(defgeneric numbytes-sparse-1 (entry)
  (:documentation "Return the numbytes of the second sparse block of the ENTRY (an integer)."))

(defgeneric offset-sparse-2 (entry)
  (:documentation "Return the offset of the third sparse block of the ENTRY (an integer)."))

(defgeneric numbytes-sparse-2 (entry)
  (:documentation "Return the numbytes of the third sparse block of the ENTRY (an integer)."))

(defgeneric offset-sparse-3 (entry)
  (:documentation "Return the offset of the fourth sparse block of the ENTRY (an integer)."))

(defgeneric numbytes-sparse-3 (entry)
  (:documentation "Return the numbytes of the fourth sparse block of the ENTRY (an integer)."))

(defgeneric isextended (entry)
  (:documentation "Return the isextended field of the ENTRY (an integer)."))

(defgeneric realsize (entry)
  (:documentation "Return the realsize of the ENTRY (an integer)."))

(defgeneric entry-file-p (entry)
  (:documentation "Returns non-NIL if ENTRY denotes a regular file.")
  (:method (entry)
    nil))

(defgeneric entry-directory-p (entry)
  (:documentation "Returns non-NIL if ENTRY denotes a directory.")
  (:method (entry)
    nil))

(defgeneric entry-symbolic-link-p (entry)
  (:documentation "Returns non-NIL if ENTRY denotes a symbolic link.")
  (:method (entry)
    nil))

(defgeneric entry-character-device-p (entry)
  (:documentation "Returns non-NIL if ENTRY denotes a character device.")
  (:method (entry)
    nil))

(defgeneric entry-block-device-p (entry)
  (:documentation "Returns non-NIL if ENTRY denotes a block device.")
  (:method (entry)
    nil))

(defgeneric entry-fifo-p (entry)
  (:documentation "Returns non-NIL if ENTRY denotes a fifo.")
  (:method (entry)
    nil))

(defgeneric entry-pax-extended-attributes-p (entry)
  (:documentation "Returns non-NIL if ENTRY contains PAX extended attributes.")
  (:method (entry)
    nil))

(defgeneric entry-pax-global-attributes-p (entry)
  (:documentation "Returns non-NIL if ENTRY contains PAX global attributes.")
  (:method (entry)
    nil))

(defgeneric entry-gnu-long-link-name-p (entry)
  (:documentation "Returns non-NIL if ENTRY contains a GNU long link name.")
  (:method (entry)
    nil))

(defgeneric entry-gnu-long-name-p (entry)
  (:documentation "Returns non-NIL if ENTRY contains a GNU long name.")
  (:method (entry)
    nil))

(defgeneric entry-gnu-directory-dump-p (entry)
  (:documentation "Returns non-NIL if ENTRY contains a GNU directory dump.")
  (:method (entry)
    nil))

(defgeneric entry-gnu-sparse-file-p (entry)
  (:documentation "Returns non-NIL if ENTRY contains a GNU sparse file.")
  (:method (entry)
    nil))

(defgeneric entry-gnu-volume-header-name-p (entry)
  (:documentation "Returns non-NIL if ENTRY contains a GNU volume header name.")
  (:method (entry)
    nil))

(defgeneric entry-unknown-p (entry)
  (:documentation "Returns non-NIL if ENTRY is unknown.")
  (:method (entry)
    nil))

;; reading
(defgeneric read-entry (tar-file)
  (:documentation "Return the next entry in TAR-FILE or NIL if there is no
next entry"))

;; writing
(defgeneric write-entry (tar-file entry
                         &key stream)
  (:documentation "Write ENTRY to TAR-FILE. Data associated with ENTRY is
written to TAR-FILE according to the :STREAM argument.  If :STREAM is T, the
expression (NAME ENTRY) is expected to refer to an existing file from which
data may be read.  If :STREAM is a stream, then data is read from that stream
and written to TAR-FILE.  If :STREAM is NIL, then no entry data is written."))

(defgeneric write-header-to-buffer (header buffer encoding &optional start)
  (:documentation "Write the information associated with HEADER into BUFFER,
beginning at position START."))

(defgeneric write-entry-data (tar-file entry stream)
  (:documentation "Write any data associated with ENTRY, possibly found
in STREAM to TAR-FILE; called after WRITE-HEADER-TO-BUFFER.  STREAM is
interpreted as in WRITE-ENTRY."))

(defgeneric finalize-tar-file (tar-file)
  (:documentation "Perform any necessary processing for finalizing TAR-FILE.
This function must be called prior to calling CLOSE-TAR-FILE."))

(defgeneric write-file-entry (tar-file name &rest args &key uname gname mode mtime uid gid size data
                                                         prefix)
  (:documentation
   "Write a FILE-ENTRY to TAR-FILE.

DATA can be either NIL (no data is written), an octet vector (written as is), a
string (encoded using UTF-8 and written), or a PATHNAME (opened, read, and
written to the archive)."))

(defgeneric write-hard-link-entry (tar-file name &rest args &key uname gname mode mtime uid gid linkname prefix)
  (:documentation
   "Write a HARD-LINK-ENTRY to TAR-FILE."))

(defgeneric write-symbolic-link-entry (tar-file name &rest args &key uname gname mode mtime uid gid linkname prefix)
  (:documentation
   "Write a SYMBOLIC-LINK-ENTRY to TAR-FILE."))

(defgeneric write-character-device-entry (tar-file name &rest args &key uname gname mode mtime uid gid
                                                                     devmajor devminor
                                                                     prefix)
  (:documentation
   "Write a CHARACTER-DEVICE-ENTRY to TAR-FILE."))

(defgeneric write-block-device-entry (tar-file name &rest args &key uname gname mode mtime uid gid
                                                                 devmajor devminor
                                                                 prefix)
  (:documentation
   "Write a BLOCK-DEVICE-ENTRY to TAR-FILE."))

(defgeneric write-directory-entry (tar-file name &rest args &key uname gname mode mtime uid gid size
                                                              prefix)
  (:documentation
   "Write a DIRECTORY-ENTRY to TAR-FILE."))

(defgeneric write-fifo-entry (tar-file name &rest args &key uname gname mode mtime uid gid prefix)
  (:documentation
   "Write a FIFO-ENTRY to TAR-FILE."))

(defgeneric write-pax-extended-attributes-entry (tar-file name &rest args &key attributes)
  (:documentation
   "Write a PAX-EXTENDED-ATTRIBUTES-ENTRY to TAR-FILE.

ATTRIBUTES must be either a hash table mapping strings to strings or an alist
mapping strings to strings. If it is an alist, ordering is preserved."))

(defgeneric write-pax-global-attributes-entry (tar-file name &rest args &key attributes)
  (:documentation
   "Write a PAX-GLOBAL-ATTRIBUTES-ENTRY to TAR-FILE.

ATTRIBUTES must be either a hash table mapping strings to strings or an alist
mapping strings to strings. If it is an alist, ordering is preserved."))

(defgeneric write-gnu-long-link-name-entry (tar-file name &rest args &key data)
  (:documentation
   "Write a GNU-LONG-LINK-NAME-ENTRY to TAR-FILE.

DATA must be either a string (which is then UTF-8 encoded) or a byte vector."))

(defgeneric write-gnu-long-name-entry (tar-file name &rest args &key data)
  (:documentation
   "Write a GNU-LONG-NAME-ENTRY to TAR-FILE.

DATA must be either a string (which is then UTF-8 encoded) or a byte vector."))

;;; Tar File
(defvar *type-detectors* nil
  "A list of functions, that when called with a header buffer must return a
  symbol naming the type of tar-file that the header belongs to, or NIL.")

(defparameter *default-type* 'v7-tar-file
  "The default tar-file type if no detectors register a hit.")

(defun register-type-detector (f)
  (pushnew f *type-detectors*))

(defun detect-type (buffer)
  (or (some (lambda (f) (funcall f buffer)) *type-detectors*)
      *default-type*))

(defclass tar-file ()
  ((direction
    :initarg :direction
    :reader %tar-file-direction
    :type (member :input :output))
   (open-tar-file-p
    :initform t
    :accessor open-tar-file-p)
   (stream
    :initarg :stream
    :reader tar-file-stream
    :type stream)
   (other-streams-to-close
    :initarg :other-streams-to-close
    :reader tar-file-other-streams-to-close
    :type list)
   (next-entry-start
    :accessor next-entry-start
    :type integer
    :initform 0)
   (header-encoding
    :initform :utf-8
    :initarg :header-encoding
    :accessor header-encoding))
  (:documentation
   "Base class of a tar file."))

(defgeneric entry-type (tar-file header)
  (:documentation
   "Return a symbol naming the class to use to represent the entry for HEADER in TAR-FILE."))

(defun make-compression-stream (stream direction compression)
  (ecase compression
    (:zstd
     (ecase direction
       (:input (io/flate:make-decompressing-stream :zstd stream))
       (:output (inspect (io/flate:make-compressing-stream :zstd stream)))))
    (:auto
     (let ((file-name (ignore-errors (pathname stream))))
       (ecase direction
         (:output
          (if (null file-name)
              stream
              (let ((type (pathname-type file-name)))
                (if (or (null type) (not (uiop:string-suffix-p type "zst")))
                    stream
                    (make-compression-stream stream direction :zstd)))))
         (:input 
          (if (null file-name)
              stream
              (let ((type (pathname-type file-name)))
                (if (or (null type) (not (uiop:string-suffix-p type "zst")))
                    stream
                    (make-compression-stream stream direction :zstd))))))))
    ((nil) stream)))

(defun open-tar-file (stream &key (direction :input)
                               (type :auto)
                               (blocking-factor 20)
                               (header-encoding :utf-8)
                               (compression :auto))
  "Create a TAR-FILE object backed by STREAM. The STREAM should not be read
from or written to any more.

DIRECTION is either :INPUT or :OUTPUT.

BLOCKING-FACTOR is an integer that specifies how many 512-byte blocks should be
read from or written to STREAM at any one time.

TYPE is either AUTO or a class designator for a subclass of TAR-FILE. If :AUTO,
the appropriate class will be determined by looking at the first tar header.

HEADER-ENCODING is an encoding specifier recognized by Babel.

COMPRESSION determines what compression scheme is used, if any. It can be
either :AUTO (the default), NIL (no compression), or :ZSTD. If :AUTO, the
compression type is determined using the PATHNAME of the stream (for :OUTPUT)
or by peeking at the stream for magic numbers (for :INPUT)."
  (declare (type (member :input :output) direction))
  (check-type compression (member :gzip :zstd :auto nil))
  (multiple-value-bind
        (compression-stream other-streams-to-close)
      (make-compression-stream stream direction compression)
    (let ((blocked-stream (make-instance (case direction
                                           (:input 'blocked-input-stream)
                                           (:output 'blocked-output-stream))
                                         :stream compression-stream
                                         :block-size (* *tar-block-bytes* blocking-factor))))
      (flet ((read-buffer ()
               (let ((buffer (make-array *tar-block-bytes* :initial-element 0
                                                             :element-type '(unsigned-byte 8))))
                 (assert (= *tar-block-bytes* (read-sequence buffer blocked-stream)))
                 buffer)))
        (make-instance (if (and (eql type :auto) (eql direction :input))
                           (detect-type
                            (read-buffer))
                           *default-type*)
          :stream blocked-stream
          :other-streams-to-close (append (unless (eql compression-stream stream)
                                            (list compression-stream))
                                          other-streams-to-close)
          :direction direction
          :header-encoding header-encoding)))))

(defmethod close-tar-file (tar-file)
  (when (open-tar-file-p tar-file)
    (close (tar-file-stream tar-file))
    (mapc #'close (tar-file-other-streams-to-close tar-file))
    (setf (open-tar-file-p tar-file) nil))
  t)

(defmethod read-entry :before ((tar-file tar-file))
  (unless (eq (%tar-file-direction tar-file) :input)
    (error "Attempting to read from a non-input tar-file"))
  (unless (open-tar-file-p tar-file)
    (error "Attempting to read from a closed tar-file")))

(defmethod write-entry :before ((tar-file tar-file) entry
                                &key stream)
  (declare (ignore stream))
  (unless (eq (%tar-file-direction tar-file) :output)
    (error "Attempting to write to a non-output tar-file"))
  (unless (open-tar-file-p tar-file)
    (error "Attempting to write to a closed tar-file")))

(defmethod write-entry-data ((tar-file tar-file) entry stream)
  (cond
    ((typep stream 'stream)
     (if (and (subtypep (stream-element-type stream) '(unsigned-byte 8))
	          (subtypep '(unsigned-byte 8) (stream-element-type stream)))
         (transfer-stream-to-tar-file tar-file stream)
         (error "Stream has invalid STREAM-ELEMENT-TYPE ~A"
                (stream-element-type stream))))
    ((typep stream 'pathname)
     (with-open-file (stream stream :element-type '(unsigned-byte 8))
       (transfer-stream-to-tar-file tar-file stream)))
    ((typep stream 'string)
     (transfer-octets-to-tar-file tar-file (string-to-octets stream :external-format :utf-8)))
    ((typep stream 'vector)
     (transfer-octets-to-tar-file tar-file stream))
    ((eq nil stream)
     ;; do nothing
     )
    (t
     (error "Invalid argument for :STREAM: ~A" stream))))

(defmethod write-entry ((tar-file tar-file) entry
                        &key stream)
  (with-slots ((tar-file-stream stream)) tar-file
    (let ((buffer (make-array *tar-block-bytes* :element-type '(unsigned-byte 8))))
      (declare (dynamic-extent buffer))
      ;; write the entry
      (write-header-to-buffer entry buffer (header-encoding tar-file) 0)
      (write-sequence buffer tar-file-stream))
    ;; write any associated data
    (write-entry-data tar-file entry stream)
    (values)))

;;; providing streamy access for an entry
(defun make-stream-for-entry (tar-file entry)
  (make-bound-stream (tar-file-stream tar-file) (size entry)))

(defmethod read-entry :before ((tar-file tar-file))
  (unless (file-position (tar-file-stream tar-file) (next-entry-start tar-file))
    (simple-tar-error "Unable to set FILE-POSITION.")))

(defmethod read-entry ((tar-file tar-file))
  (let ((start-position (file-position (tar-file-stream tar-file)))
        (buffer (make-array *tar-block-bytes* :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent buffer))
    (with-slots (stream) tar-file
      (let ((nbytes (read-sequence buffer stream)))
        (unless (= nbytes *tar-block-bytes*)
          (error "Corrupt tar-file"))))
    (if (null-block-p buffer 0)
        nil
        (let ((header (read-header-from-buffer (header-type tar-file) buffer
                                               (header-encoding tar-file)
                                               :start 0)))
          (make-instance (entry-type tar-file header)
                         :tar-file tar-file
                         :header header
                         :start start-position)))))

(defmethod read-entry :around ((tar-file tar-file))
  (let ((entry (call-next-method)))
    (unless (null entry)
      (setf (next-entry-start tar-file)
            (+ (start entry)
               *tar-block-bytes*
               (if (entry-has-data-p entry)
                   (round-up-to-tar-block (size entry))
                   0))))
    entry))

(defun transfer-stream-to-tar-file (tar-file stream)
  (let* ((bytes-copied (copy-stream stream (tar-file-stream tar-file)))
         (rounded-bytes (round-up-to-tar-block bytes-copied))
         (bytes-remaining (- rounded-bytes bytes-copied)))
    (write-sequence (make-array bytes-remaining :element-type '(unsigned-byte 8)
                                                :initial-element 0)
                    (tar-file-stream tar-file))))

(defun transfer-octets-to-tar-file (tar-file octets)
  (let* ((rounded-bytes (round-up-to-tar-block (length octets)))
         (bytes-remaining (- rounded-bytes (length octets))))
    (write-sequence octets (tar-file-stream tar-file))
    (write-sequence (make-array bytes-remaining :element-type '(unsigned-byte 8)
                                                :initial-element 0)
                    (tar-file-stream tar-file))))

(defmethod finalize-tar-file ((tar-file tar-file))
  (let ((null-block (make-array *tar-block-bytes*
                                :element-type '(unsigned-byte 8)
                                :initial-element 0)))
    (declare (dynamic-extent null-block))
    (dotimes (i 2)
      (write-sequence null-block (tar-file-stream tar-file)))
    (values)))

(define-octet-header v7-header
    (name 100 :string-null)
  (mode 8 :octnum)
  (uid 8 :octnum)
  (gid 8 :octnum)
  (size 12 :octnum)
  (mtime 12 :octnum)
  (checksum 8 :octnum)
  (typeflag 1 :byte)
  (linkname 100 :string-null)
  ;; not part of the tar format, but it makes defined constants come out right
  (%%padding 255 :string))

(defclass v7-tar-file (tar-file) ()
  (:documentation
   "A v7 tar file."))

(defmethod header-type ((tar-file v7-tar-file))
  'v7-header)

(defmethod entry-type ((tar-file v7-tar-file) header)
  (if (ends-with-subseq "/" (name header))
      'tar-directory-entry
      (switch ((typeflag header))
        (+tar-regular-file+
         'tar-file-entry)
        (+tar-regular-alternate-file+
         'tar-file-entry)
        (+tar-hard-link+
         'tar-hard-link-entry)
        (+tar-symbolic-link+
         'tar-symbolic-link-entry)
        (+tar-directory-file+
         'tar-directory-entry)
        (t
         'unknown-tar-entry))))

(defparameter *ustar-magic-vector*
  (coerce `(,@(map 'list #'char-code "ustar") 0)
          '(vector (unsigned-byte 8)))
  "The contents of the magic field for ustar tar-files.")

(defparameter *ustar-version-vector*
  (coerce (map 'list #'char-code "00") '(vector (unsigned-byte 8)))
  "The contents of the version field for ustar tar-files.")

;;; definitions taken from the FreeBSD 5.1 manpage
(define-octet-header ustar-header
    (name 100 :string-null)
  (mode 8 :octnum)
  (uid 8 :octnum)
  (gid 8 :octnum)
  (size 12 :octnum)
  (mtime 12 :octnum)
  (checksum 8 :octnum)
  (typeflag 1 :byte)
  (linkname 100 :string-null)
  (magic 6 :bytes *ustar-magic-vector*)
  (version 2 :bytes *ustar-version-vector*)
  ;; to be used in preference to uid and gid, of course
  (uname 32 :string-null)
  (gname 32 :string-null)
  (devmajor 8 :octnum)
  (devminor 8 :octnum)
  (prefix 155 :string-null)
  ;; not part of the tar format, but it makes defined constants come out right
  (%%padding 12 :string))

(defclass ustar-tar-file (tar-file) ()
  (:documentation
   "A ustar tar file."))

(defmethod header-type ((tar-file ustar-tar-file))
  'ustar-header)

(defun detect-ustar-tar-file (buffer)
  (let ((offset (field-offset 'ustar-header 'magic))
        (length (field-length 'ustar-header 'magic)))
    (when (equalp *ustar-magic-vector*
                  (subseq buffer offset (+ offset length)))
      'ustar-tar-file)))

(register-type-detector 'detect-ustar-tar-file)

(defmethod entry-type ((tar-file ustar-tar-file) header)
  (switch ((typeflag header))
    (+tar-regular-file+
     'tar-file-entry)
    (+tar-regular-alternate-file+
     'tar-file-entry)
    (+tar-hard-link+
     'tar-hard-link-entry)
    (+tar-symbolic-link+
     'tar-symbolic-link-entry)
    (+tar-character-device+
     'tar-character-device-entry)
    (+tar-block-device+
     'tar-block-device-entry)
    (+tar-directory-file+
     'tar-directory-entry)
    (+tar-fifo-device+
     'tar-fifo-entry)
    (+posix-extended-header+
     'pax-extended-attributes-entry)
    (+posix-global-header+
     'pax-global-attributes-entry)
    (t
     'unknown-tar-entry)))

(defparameter *gnu-magic-vector*
  (coerce `(,@(map 'list #'char-code "ustar "))
          '(vector (unsigned-byte 8)))
  "The contents of the magic field for gnu tar-files.")

(defparameter *gnu-version-vector*
  (coerce `(,@(map 'list #'char-code " ") 0) '(vector (unsigned-byte 8)))
  "The contents of the version field for gnu tar-files.")

(define-octet-header gnu-header
    (name 100 :string-null)
  (mode 8 :octnum)
  (uid 8 :octnum)
  (gid 8 :octnum)
  (size 12 :octnum)
  (mtime 12 :octnum)
  (checksum 8 :octnum)
  (typeflag 1 :byte)
  (linkname 100 :string-null)
  (magic 6 :string *gnu-magic-vector*)
  (version 2 :string *gnu-version-vector*)
  (uname 32 :string-null)
  (gname 32 :string-null)
  (devmajor 8 :octnum)
  (devminor 8 :octnum)
  (atime 12 :octnum)
  (ctime 12 :octnum)
  (offset 12 :octnum)
  (longnames 4 :string)
  (unused 1 :byte)
  (offset-sparse-0 12 :octnum)
  (numbytes-sparse-0 12 :octnum)
  (offset-sparse-1 12 :octnum)
  (numbytes-sparse-1 12 :octnum)
  (offset-sparse-2 12 :octnum)
  (numbytes-sparse-2 12 :octnum)
  (offset-sparse-3 12 :octnum)
  (numbytes-sparse-3 12 :octnum)
  (isextended 1 :byte)
  (realsize 12 :octnum)
  (%%padding 17 :string))

(defclass gnu-tar-file (tar-file) ()
  (:documentation "A gnu tar file."))

(defmethod header-type ((tar-file gnu-tar-file))
  'gnu-header)

(defun detect-gnu-tar-file (buffer)
  (let ((offset (field-offset 'gnu-header 'magic))
        (length (field-length 'gnu-header 'magic)))
    (when (equalp *gnu-magic-vector*
                  (subseq buffer offset (+ offset length)))
      'gnu-tar-file)))

(register-type-detector 'detect-gnu-tar-file)

(defmethod entry-type ((tar-file gnu-tar-file) header)
  (switch ((typeflag header))
    (+tar-regular-file+
     'tar-file-entry)
    (+tar-regular-alternate-file+
     'tar-file-entry)
    (+tar-hard-link+
     'tar-hard-link-entry)
    (+tar-symbolic-link+
     'tar-symbolic-link-entry)
    (+tar-character-device+
     'tar-character-device-entry)
    (+tar-block-device+
     'tar-block-device-entry)
    (+tar-directory-file+
     'tar-directory-entry)
    (+tar-fifo-device+
     'tar-fifo-entry)
    (+gnutar-long-name+
     'gnu-long-name-entry)
    (+gnutar-long-link-name+
     'gnu-long-link-name-entry)
    (+gnutar-sparse+
     'gnu-sparse-file-entry)
    (+gnutar-directory-dump+
     'gnu-directory-dump-entry)
    (+gnutar-volume-header-name+
     'gnu-volume-header-name-entry)
    (t
     'unknown-tar-entry)))

;;; Entries
(defclass archive () ())
(defclass tar-archive (archive) ())
(defclass tar-entry ()
  ((tar-file
    :initarg :tar-file
    :reader tar-file)
   (start
    :initarg :start
    :reader start
    :documentation
    "The FILE-POSITION of the start of the entry.")
   (header
    :initarg :header
    :reader header))
  (:documentation
   "Base class for all entries in a tar file."))

(defclass tar-entry-data () ())
(defclass tar-file-entry (tar-entry tar-entry-data) ()
  (:documentation
   "A regular file."))

(defun read-number-from-buffer (buffer &key (start 0) end (radix 10))
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer))
  (declare (type (integer 2 36) radix))
  (let ((end (or (position-if #'(lambda (b)
                                  ;; For BSD tar, a number can end with
                                  ;; a space or a null byte.
                                  (or (= b +ascii-space+) (zerop b)))
                              buffer :start start :end end)
                 end
                 (length buffer))))
    ;; GNU tar permits storing numbers as binary; a binary number is
    ;; indicated by starting the field with #x80.
    (if (= (aref buffer start) #x80)
        (loop for i from (1- end) downto (1+ start)
              for base = 1 then (* base 256)
              sum (* (aref buffer i) base))
        (loop for i from (1- end) downto start
              for base = 1 then (* base radix)
              sum (let ((byte (aref buffer i)))
                    (cond
                      ((<= +ascii-zero+ byte +ascii-nine+)
                       (* base (- byte +ascii-zero+)))
                      ((<= +ascii-a+ byte +ascii-z+)
                       (* base (+ 10 (- byte +ascii-a+))))
                      (t (simple-tar-error "Invalid byte: ~A in ~A"
                                           byte (subseq buffer start end)))))))))

(defun write-number-to-buffer (number buffer
                               &key (start 0) end (radix 10) nullp)
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer))
  (declare (type (integer 2 36) radix))
  (let ((end (let ((dend (or end (length buffer))))
               (if nullp
                   (1- dend)
                   dend))))
    (loop for i from (1- end) downto start
          do (multiple-value-bind (quo rem) (truncate number radix)
               (setf number quo)
               (setf (aref buffer i)
                     (cond
                       ((<= 0 rem 9) (+ rem +ascii-zero+))
                       ((<= 10 rem 36) (+ (- rem 10) +ascii-a+))
                       (t (simple-tar-error "Don't know how to encode ~A" rem))))))
    (values)))

(defun read-octets-from-buffer (buffer &key (start 0) end nullp)
  (let ((end (if nullp
                 (or (position 0 buffer :start start :end end) end)
                 end)))
    (subseq buffer start end)))

(defmethod write-file-entry (tar-file name &rest args &key uname gname mode mtime uid gid size data
                                                        prefix)
  (declare (ignore uname gname mode mtime uid gid prefix))
  ;; Compute the size when necessary.
  (let ((computed-size
          (etypecase data
            (string
             (setf data (string-to-octets data :external-format :utf-8))
             (push data args)
             (push :data args)
             (length data))
            (vector
             (length data))
            (pathname
             (with-open-file (s data :element-type '(unsigned-byte 8))
               (file-length s)))
            (null
             0)
            (stream
             (file-length data)))))
    (when (not (null computed-size))
      (cond
        ((null size)
         (setf size computed-size))
        ((/= size computed-size)
         (error 'simple-tar-file-error
                :format-control "Computed (~A) and specified (~A) sizes mismatch"
                :format-args (list computed-size size))))))
  (when (null size)
    (error 'simple-tar-file-error
           :format-control "Size not provided and unable to compute it."))
  (push size args)
  (push :size args)

  (let ((header (apply #'make-instance (header-type tar-file)
                       :name name
                       :typeflag (if (typep tar-file 'v7-tar-file)
                                     +tar-regular-alternate-file+
                                     +tar-regular-file+)
                       (uiop:remove-plist-key :data args)))
        (start-position (file-position (tar-file-stream tar-file))))
    (write-entry tar-file header :stream data)
    (make-instance 'tar-file-entry :header header :tar-file tar-file :start start-position)))

(defclass tar-hard-link-entry (tar-entry)
  ()
  (:documentation
   "A hard link."))

(defmethod write-hard-link-entry (tar-file name &rest args &key uname gname mode mtime uid gid linkname prefix)
  (declare (ignore uname gname mode mtime uid gid linkname prefix))
  (let ((header (apply #'make-instance (header-type tar-file)
                       :name name
                       :typeflag +tar-hard-link+
                       args))
        (start-position (file-position (tar-file-stream tar-file))))
    (write-entry tar-file header)
    (make-instance 'tar-hard-link-entry :header header :tar-file tar-file :start start-position)))

(defclass tar-symbolic-link-entry (tar-entry)
  ()
  (:documentation
   "A symbolic link."))

(defmethod write-symbolic-link-entry (tar-file name &rest args &key uname gname mode mtime uid gid linkname prefix)
  (declare (ignore uname gname mode mtime uid gid linkname prefix))
  (let ((header (apply #'make-instance (header-type tar-file)
                       :name name
                       :typeflag +tar-symbolic-link+
                       args))
        (start-position (file-position (tar-file-stream tar-file))))
    (write-entry tar-file header)
    (make-instance 'tar-symbolic-link-entry :header header :tar-file tar-file :start start-position)))

(defclass tar-character-device-entry (tar-entry)
  ()
  (:documentation
   "A character device."))

(defmethod write-character-device-entry (tar-file name &rest args &key uname gname mode mtime uid gid
                                                                    devmajor devminor
                                                                    prefix)
  (declare (ignore uname gname mode mtime uid gid devmajor devminor prefix))
  (let ((header (apply #'make-instance (header-type tar-file)
                       :name name
                       :typeflag +tar-character-device+
                       args))
        (start-position (file-position (tar-file-stream tar-file))))
    (write-entry tar-file header)
    (make-instance 'tar-character-device-entry :header header :tar-file tar-file :start start-position)))

(defclass tar-block-device-entry (tar-entry)
  ()
  (:documentation
   "A block device."))

(defmethod write-block-device-entry (tar-file name &rest args &key uname gname mode mtime uid gid
                                                                devmajor devminor
                                                                prefix)
  (declare (ignore uname gname mode mtime uid gid devmajor devminor prefix))
  (let ((header (apply #'make-instance (header-type tar-file)
                       :name name
                       :typeflag +tar-block-device+
                       args))
        (start-position (file-position (tar-file-stream tar-file))))
    (write-entry tar-file header)
    (make-instance 'tar-block-device-entry :header header :tar-file tar-file :start start-position)))

(defclass tar-directory-entry (tar-entry)
  ()
  (:documentation
   "A directory."))

(defmethod write-directory-entry (tar-file name &rest args &key uname gname mode mtime uid gid size
                                                             prefix)
  (declare (ignore uname gname mode mtime uid gid size prefix))
  (let ((header (apply #'make-instance (header-type tar-file)
                       :name name
                       :typeflag +tar-directory-file+
                       args))
        (start-position (file-position (tar-file-stream tar-file))))
    (write-entry tar-file header)
    (make-instance 'tar-directory-entry :header header :tar-file tar-file :start start-position)))

(defclass tar-fifo-entry (tar-entry)
  ()
  (:documentation
   "A FIFO."))

(defmethod write-fifo-entry (tar-file name &rest args &key uname gname mode mtime uid gid prefix)
  (declare (ignore uname gname mode mtime uid gid prefix))
  (let ((header (apply #'make-instance (header-type tar-file)
                       :name name
                       :typeflag +tar-fifo-device+
                       args))
        (start-position (file-position (tar-file-stream tar-file))))
    (write-entry tar-file header)
    (make-instance 'tar-fifo-entry :header header :tar-file tar-file :start start-position)))

(defclass pax-attributes-entry (tar-entry tar-entry-data)
  ((attributes
    :accessor attributes
    :documentation
    "A hash table mapping attribute names (strings) to values (strings).")))

(defgeneric attribute (entry name &optional default)
  (:documentation
   "Get the NAME attribute from ENTRY."))

(defmethod attribute ((entry pax-attributes-entry) name &optional default)
  (gethash name (attributes entry) default))

(defgeneric attribute-names (entry)
  (:documentation
   "Return a list of attribute names contained within ENTRY."))

(defmethod attribute-names ((entry pax-attributes-entry))
  (hash-table-values (attributes entry)))

(defmacro do-attributes ((name value entry &optional result) &body body)
  "Given a PAX ENTRY with attributes, execute BODY for every attribute, with
NAME bound to the attribute name and VALUE bound to the attribute value."
  `(block nil
     (maphash
      (lambda (,name ,value)
        ,@body)
      (attributes ,entry))
     ,result))

(defun read-attribute-length (stream)
  "Pop bytes out of the buffer until a space is read, then try turning that
  into a number."
  (let* ((bytes-read 0)
         (bytes (loop
                  :for byte := (read-byte stream nil :eof)
                  :when (eql byte :eof)
                    :do (return :eof)
                  :end
                  :do (incf bytes-read)
                  :until (eql byte +ascii-space+)
                  :collect byte)))
    (if (eql bytes :eof)
        :eof
        (values (parse-integer (octets-to-string (coerce bytes '(vector (unsigned-byte 8)))
                                                 :external-format :utf-8))
                bytes-read))))

(defun read-attribute (stream)
  (multiple-value-bind (num-bytes bytes-read)
      (read-attribute-length stream)
    (let (buffer
          num-read
          kv-string
          =-position)
      (unless (eql num-bytes :eof)
        (setf buffer (make-array (- num-bytes bytes-read) :element-type '(unsigned-byte 8) :initial-element 0))
        (setf num-read (read-sequence buffer stream))
        (unless (= num-read (- num-bytes bytes-read))
          (error 'malformed-pax-attribute-entry))
        (setf kv-string (octets-to-string buffer :external-format :utf-8))
        (unless (= (aref buffer (1- num-read)) +ascii-newline+)
          (error 'malformed-pax-attribute-entry))
        (setf =-position (position #\= kv-string))
        (when (null =-position)
          (error 'malformed-pax-attribute-entry))
        (values (subseq kv-string 0 =-position)
                (subseq kv-string (1+ =-position) (1- num-read))
                t)))))

(defun populate-pax-attributes (entry)
  (let ((stream (make-entry-stream entry))
        (ht (make-hash-table :test 'equal)))
    (loop
      (multiple-value-bind (key value exists-p)
          (read-attribute stream)
        (unless exists-p (return))
        (setf (gethash key ht) value)))
    (setf (attributes entry) ht)))

(defmethod slot-unbound (class (entry pax-attributes-entry) (slot-name (eql 'attributes)))
  (populate-pax-attributes entry))

(defclass pax-extended-attributes-entry (pax-attributes-entry)
  ()
  (:documentation
   "Extended attributes for the subsequent record."))

(defmethod user-attributes-to-alist ((attributes hash-table))
  (hash-table-alist attributes))

(defmethod user-attributes-to-alist ((attributes list))
  attributes)

(defun attribute-pair-to-octets (pair)
  (let* ((pair-string (concatenate 'string " "
                                   (car pair)
                                   "="
                                   (cdr pair)
                                   (list #\Linefeed)))
         (pair-vector (string-to-octets pair-string :external-format :utf-8))
         (base-length (length pair-vector)))
    (loop
      :for offset :upfrom 0 :below 2
      :for estimated-length := (+ offset (ceiling (log base-length 10)) base-length)
      :for length-vector := (string-to-octets (prin1-to-string estimated-length)
                                               :external-format :utf-8)
      :when (= estimated-length (+ (length length-vector) base-length))
        :return (concatenate '(vector (unsigned-byte 8)) length-vector pair-vector))))

(defun attribute-alist-to-octets (alist)
  (apply #'concatenate '(vector (unsigned-byte 8)) (mapcar #'attribute-pair-to-octets alist)))

(defmethod write-pax-extended-attributes-entry (tar-file name &rest args &key attributes)
  (let* ((alist (user-attributes-to-alist attributes))
         (data (attribute-alist-to-octets alist))
         (size (length data)))
    (let ((header (apply #'make-instance (header-type tar-file)
                         :name name
                         :typeflag +posix-extended-header+
                         :size size
                         (remf args :attributes)))
          (start-position (file-position (tar-file-stream tar-file))))
      (write-entry tar-file header :stream data)
      (make-instance 'tar-directory-entry :header header :tar-file tar-file :start start-position))))

(defmethod entry-pax-extended-attributes-p ((entry pax-extended-attributes-entry))
  t)

(defclass pax-global-attributes-entry (pax-attributes-entry)
  ()
  (:documentation
   "Extended attributes for all subsequent records."))

(defmethod write-pax-global-attributes-entry (tar-file name &rest args &key attributes)
  (let* ((alist (user-attributes-to-alist attributes))
         (data (attribute-alist-to-octets alist))
         (size (length data)))
    (let ((header (apply #'make-instance (header-type tar-file)
                         :name name
                         :typeflag +posix-global-header+
                         :size size
                         (remf args :attributes)))
          (start-position (file-position (tar-file-stream tar-file))))
      (write-entry tar-file header :stream data)
      (make-instance 'tar-directory-entry :header header :tar-file tar-file :start start-position))))

(defmethod entry-pax-global-attributes-p ((entry pax-global-attributes-entry))
  t)

(defclass gnu-directory-dump-entry (tar-entry tar-entry-data)
  ())

(defmethod entry-gnu-directory-dump-p ((entry gnu-directory-dump-entry))
  t)

(defclass gnu-long-link-name-entry (tar-entry tar-entry-data)
  ((long-link-name
    :accessor long-link-name)))

(defmethod slot-unbound (class (entry gnu-long-link-name-entry) (slot-name (eql 'long-link-name)))
  (let ((buffer (make-array (size entry) :element-type '(unsigned-byte 8)
                                         :initial-element 0))
        (stream (make-entry-stream entry)))
    (read-sequence buffer stream)
    (setf (long-link-name entry) (octets-to-string buffer :external-format :utf-8))))

(defmethod write-gnu-long-link-name-entry (tar-file name &rest args &key data)
  (let* ((data (etypecase data
                 (string
                  (string-to-octets data :external-format :utf-8))
                 ((vector (unsigned-byte 8))
                  data)))
         (size (length data))
         (header (apply #'make-instance (header-type tar-file)
                        :name name
                        :typeflag +gnutar-long-link-name+
                        :size size
                        (remf args :data)))
         (start-position (file-position (tar-file-stream tar-file))))
    (write-entry tar-file header :stream data)
    (make-instance 'gnu-long-link-name-entry
                   :header header
                   :tar-file tar-file
                   :start start-position)))

(defmethod entry-gnu-long-link-name-p ((entry gnu-long-link-name-entry))
  t)

(defclass gnu-long-name-entry (tar-entry tar-entry-data)
  ((long-name
    :accessor long-name)))

(defmethod slot-unbound (class (entry gnu-long-name-entry) (slot-name (eql 'long-name)))
  (let ((buffer (make-array (size entry) :element-type '(unsigned-byte 8)
                                         :initial-element 0))
        (stream (make-entry-stream entry)))
    (read-sequence buffer stream)
    (setf (long-name entry) (octets-to-string buffer :external-format :utf-8))))

(defmethod write-gnu-long-name-entry (tar-file name &rest args &key data)
  (let* ((data (etypecase data
                 (string
                  (string-to-octets data :external-format :utf-8))
                 ((vector (unsigned-byte 8))
                  data)))
         (size (length data))
         (header (apply #'make-instance (header-type tar-file)
                        :name name
                        :typeflag +gnutar-long-link-name+
                        :size size
                        (remf args :data)))
         (start-position (file-position (tar-file-stream tar-file))))
    (write-entry tar-file header :stream data)
    (make-instance 'gnu-long-name-entry
                   :header header
                   :tar-file tar-file
                   :start start-position)))

(defmethod entry-gnu-long-name-p ((entry gnu-long-name-entry))
  t)

(defclass gnu-sparse-file-entry (tar-entry tar-entry-data)
  ())

(defmethod entry-gnu-sparse-file-p ((entry gnu-sparse-file-entry))
  t)

(defclass gnu-volume-header-name-entry (tar-entry)
  ())

(defmethod entry-gnu-volume-header-name-p ((entry gnu-volume-header-name-entry))
  t)

(defclass unknown-tar-entry (tar-entry tar-entry-data)
  ()
  (:documentation
   "An unknown entry."))

(defmethod entry-unknown-p ((tar-entry unknown-tar-entry))
  t)

(defgeneric entry-has-data-p (entry)
  (:documentation
   "Returns non-NIL if ENTRY has associated data that can be read using MAKE-ENTRY-STREAM.")
  (:method (entry) nil)
  (:method ((entry tar-entry-data)) t))

(defgeneric make-entry-stream (entry)
  (:documentation
   "Returns a new binary stream that contains ENTRY's data."))

(defmethod make-entry-stream ((entry tar-entry-data))
  (make-bound-stream (tar-file-stream (tar-file entry)) (size entry)
                     (+ (start entry) *tar-block-bytes*)))

(defmacro make-header-forwarder (name)
  `(progn
     (defmethod ,name ((entry tar-entry))
       (,name (header entry)))))

(make-header-forwarder name)
(make-header-forwarder mode)
(make-header-forwarder uid)
(make-header-forwarder gid)
(make-header-forwarder size)
(make-header-forwarder mtime)
(make-header-forwarder checksum)
(make-header-forwarder typeflag)
(make-header-forwarder linkname)
(make-header-forwarder magic)
(make-header-forwarder version)
(make-header-forwarder uname)
(make-header-forwarder gname)
(make-header-forwarder devmajor)
(make-header-forwarder devminor)
(make-header-forwarder prefix)
(make-header-forwarder atime)
(make-header-forwarder ctime)
(make-header-forwarder offset)
(make-header-forwarder offset-sparse-0)
(make-header-forwarder numbytes-sparse-0)
(make-header-forwarder offset-sparse-1)
(make-header-forwarder numbytes-sparse-1)
(make-header-forwarder offset-sparse-2)
(make-header-forwarder numbytes-sparse-2)
(make-header-forwarder offset-sparse-3)
(make-header-forwarder numbytes-sparse-3)
(make-header-forwarder isextended)
(make-header-forwarder realsize)

(defmethod print-object ((entry tar-entry) stream)
  (print-unreadable-object (entry stream)
    (format stream "Entry ~A" (name entry))))

(defmethod entry-file-p ((entry tar-file-entry))
  t)

(defmethod entry-directory-p ((entry tar-directory-entry))
  t)

(defmethod entry-hard-link-p ((entry tar-hard-link-entry))
  t)

(defmethod entry-symbolic-link-p ((entry tar-symbolic-link-entry))
  t)

(defmethod entry-character-device-p ((entry tar-character-device-entry))
  t)

(defmethod entry-block-device-p ((entry tar-block-device-entry))
  t)

(defmethod entry-fifo-p ((entry tar-fifo-entry))
  t)

;;; External Macros
(defun call-with-open-tar-file (thunk pathname-or-stream
                                &key (direction :input)
                                  (if-exists nil)
                                  (if-does-not-exist :create)
                                  (type :auto)
                                  (blocking-factor 20)
                                  (compression :auto)
                                  (header-encoding :utf-8))
  (declare ((member :input :output) direction))
  (let (tar-file
        stream
        (should-close t)
        (abort t))
    (unwind-protect
         (progn
           (when (streamp pathname-or-stream) (setf should-close nil))
           (setf stream (if should-close
                            (apply #'open
                                   pathname-or-stream
                                   :direction direction
                                   :element-type '(unsigned-byte 8)
                                   (append
                                    (when if-exists
                                      (list :if-exists if-exists))
                                    (when if-does-not-exist
                                      (list :if-does-not-exist if-does-not-exist))))
                            pathname-or-stream))
           (setf tar-file (open-tar-file stream :direction direction
                                                :type type
                                                :blocking-factor blocking-factor
                                                :header-encoding header-encoding
                                                :compression compression))
           (multiple-value-prog1
               (funcall thunk tar-file)
             (setf abort nil)))
      (when tar-file
        (when (eql direction :output)
          (finalize-tar-file tar-file))
        (close-tar-file tar-file)
        (setf tar-file nil))
      (when should-close
        (close stream :abort abort)))))

(defmacro with-open-tar-file ((tar-file-var pathname-or-stream
                               &key (direction :input)
                                 (if-exists nil)
                                 (if-does-not-exist nil)
                                 (type :auto)
                                 (compression :auto)
                                 (blocking-factor 20)
                                 (header-encoding :utf-8))
                              &body body)
  "Bind TAR-FILE-VAR to a newly opened TAR-FILE, backed by
PATHNAME-OR-STREAM. If PATHNAME-OR-STREAM evaluates to a stream, that stream
is used directly, otherwise, it is opened via OPEN. If PATHNAME-OR-STREAM is a
stream, that stream is not closed upon exiting the body of the macro.

DIRECTION must be either :INPUT or :OUTPUT.

IF-EXISTS and IF-DOES-NOT-EXIST are passed to OPEN if PATHNAME-OR-STREAM is
not a stream.

See OPEN-TAR-FILE for a description of TYPE, BLOCKING-FACTOR, HEADER-ENCODING,
and COMPRESSION."
  (declare ((member :input :output) direction))
  `(call-with-open-tar-file 
    (lambda (,tar-file-var) ,@body)
    ,pathname-or-stream
    :direction ,direction
    :if-exists ,if-exists
    :if-does-not-exist ,if-does-not-exist
    :type ,type
    :blocking-factor ,blocking-factor
    :header-encoding ,header-encoding
    :compression ,compression))

(defmacro do-entries ((entry tar-file &optional result)
                      &body body)
  "Iterate over the entries in TAR-FILE.  For each entry, ENTRY is bound to an
ENTRY representing the entry.  RESULT is used as in DOTIMES."
  (let ((tar-file-var (gensym)))
    `(let ((,tar-file-var ,tar-file))
       (do ((,entry (read-entry ,tar-file-var)
                    (read-entry ,tar-file-var)))
           ((null ,entry) ,result)
         ,@body))))
