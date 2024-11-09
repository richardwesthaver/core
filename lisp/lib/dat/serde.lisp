;;; serde.lisp --- Binary Lisp Data Formats

;; Direct De/Serialization of Lisp Objects to/from OCTET vectors and streams.

;;; Code:
(in-package :dat/serde)
(declaim  (optimize speed))

(defun read-fixnum64 (bs)
  (declare (type buffer-stream bs))
  (let ((position (pos bs)))
    (declare (type fixnum position))
    (setf (pos bs) (the fixnum (+ position 8)))
    (if (< #.most-positive-fixnum +2^32+)
        ;; 32-bit or less fixnums; need to process as bignums64
        (let ((first (read-int32 (buffer bs) position))
              (second (read-int32 (buffer bs) 
                                  (the fixnum (+ position 4)))))
          (if (little-endian-p)
              (+ first (ash second 32))
              (+ second (ash first 32))))
        ;; Native 64-bit fixnums (NOTE: issues with non 32/64 bit fixnums?)
        (read-int64 (buffer bs) position))))

(defvar *buffer-streams* (make-array 0 :adjustable t :fill-pointer t)
  "Vector of buffer-streams, which you can grab / return.")

(defvar *buffer-streams-lock* (make-mutex :name "buffer-streams"))

(defclass buffer-stream (wrapped-stream)
  ((buffer :initform (make-static-vector 10) :initarg :buffer :type alien-or-lisp-octets :accessor buffer)
   (size :initform 0 :type fixnum :initarg :size :accessor size)
   (pos :initform 0 :type fixnum :initarg :pos :accessor pos)
   (len :initform 10 :type fixnum :initarg :len :accessor len))
  (:documentation "A stream containing a static vector, providing an interface to foreign char
buffers."))

(defmethod stream-file-position ((stream buffer-stream) &optional spec)
  (if spec
      (setf (len stream) spec)
      (len stream)))

(defun grab-buffer-stream ()
  "Grab a buffer-stream from the *buffer-streams* resource pool."
  (or (with-mutex (*buffer-streams-lock*)
        (and (plusp (length *buffer-streams*))
             (vector-pop *buffer-streams*)))
      (make-instance 'buffer-stream)))

(defun return-buffer-stream (bs)
  "Return a buffer-stream to the *buffer-streams* resource pool."
  (reset-buffer-stream bs)
  (with-mutex (*buffer-streams-lock*)
    (vector-push-extend bs *buffer-streams*)))

(defmacro with-buffer-streams (names &body body)
  "Grab a buffer-stream, executes forms, and returns the
stream to the pool on exit."
  `(let ,(loop for name in names collect (list name '(grab-buffer-stream)))
     (declare (type buffer-stream ,@names))
     (unwind-protect
          (progn ,@body)
       (progn
         ,@(loop for name in names 
              collect (list 'return-buffer-stream name))))))

(defun resize-buffer-stream (bs length)
  "Resize the underlying buffer of a buffer-stream, copying the old data."
  (declare (buffer-stream bs)
           (fixnum length))
  (with-slots (buffer size len) bs
    (declare (type fixnum size len)
             (type (alien (* unsigned-char)) buffer))
    (when (> length len)
      (let ((newlen (max length (* len 2))))
        (declare (type fixnum newlen))
        ;; FIXME: async unwinds between alloc of newbuf and free of buf
        ;; will leave us with a memory leak of size NEWLEN.
        (let ((newbuf (make-alien unsigned-char newlen)))
          ;; technically we just need to copy from position to size.....
          (when (null-alien newbuf)
            (error "Failed to allocate buffer stream of length ~A.  allocate-foreign-object returned a null pointer" newlen))
          (std/alien:memcpy newbuf buffer size)
          (free-alien buffer)
          (setf buffer newbuf)
          (setf len newlen)
          nil)))))

(defun resize-buffer-stream-no-copy (bs length)
  "Resize the underlying buffer of a buffer-stream."
  (declare (buffer-stream bs)
           (fixnum length))
  (with-slots (buffer size len) bs
    (declare (fixnum size len)
             ((alien (* unsigned-char)) buffer))
    (when (> length len)
      (let ((newlen (max length (* len 2))))
        (declare (type fixnum newlen))
        ;; FIXME: async unwinds between alloc of newbuf and free of buf
        ;; will leave us with a memory leak of size NEWLEN.
        (let ((newbuf (make-alien unsigned-char newlen)))
          (when (null-alien newbuf)
            (error "Failed to allocate buffer stream of length ~A.  allocate-foreign-object returned a null pointer" newlen))
          (free-alien buffer)
          (setf buffer newbuf)
          (setf len newlen)
          nil)))))

(defun reset-buffer-stream (bs)
  "'Empty' the buffer-stream."
  (declare (type buffer-stream bs))
  (setf (size bs) 0
        (len bs) 0))

;; Constants
(defconstant +fixnum32+              1)
(defconstant +fixnum64+              2)
(defconstant +char+                  3)
(defconstant +single-float+          4)
(defconstant +double-float+          5)
(defconstant +negative-bignum+       6)
(defconstant +positive-bignum+       7)
(defconstant +rational+              8)

;; Save constants by splitting strings and encoding
(defconstant +utf8-string+           9)
(defconstant +utf16-string+         10)
(defconstant +utf32-string+         11)

;; String-based aggregates
(defconstant +pathname+             12)
(defconstant +symbol+               13)

;; Stored by ID (requires instance table)
(defconstant +stored-ref+       14)
;; Stored by id+classname
(defconstant +stored+           15)

;; Composite objects
(defconstant +cons+                 16)
(defconstant +hash-table+           17)
(defconstant +object+               18)
(defconstant +array+                19)
(defconstant +struct+               20)
(defconstant +class+                21)
(defconstant +complex+              22)
;;(defconstant +oid-pair+             23)

;; Lispworks support
(defconstant +short-float+          30)

(defconstant +nil+                  #x3F)
(defconstant +reserved-dbinfo+      #xF0)

;; Arrays
(defconstant +fill-pointer-p+     #x20)
(defconstant +adjustable-p+       #x40)

;;
;; Circularity Hash for Serializer
;;

(defparameter *circularity-initial-hash-size* 50
  "Default size of the circularity cache used in the serializer.")

(defparameter *circularity-hash-queue* (make-array 20 :fill-pointer 0 :adjustable t)
  "Circularity ids for the serializer.")

(defparameter *serializer-lock* (make-mutex))

(defun get-circularity-hash ()
  "Get a clean hash for object serialization"
  (declare (type fixnum *circularity-initial-hash-size*))
  (or
   (with-mutex (*serializer-lock*)
     (and (plusp (length *circularity-hash-queue*))
          (vector-pop *circularity-hash-queue*)))
   (make-hash-table :test 'eq :size *circularity-initial-hash-size*)))

(defun release-circularity-hash (hash)
  "Return the hash to the queue for reuse"
  (unless (= (hash-table-count hash) 0)
    (clrhash hash))
  (with-mutex (*serializer-lock*)
    (vector-push-extend hash *circularity-hash-queue*)))

;;
;; Circularity Hash for Deserializer
;;
;; NOTE: this strategy may create GC problems as it maintains references to
;; potentially large objects

(defparameter *circularity-vector-queue* (make-array 20 :fill-pointer 0 :adjustable t)
  "A list of vectors used for linear deserialization.
   This works nicely because all ID's are written
   in integer order to the stream, so we can just write
   the next one into the array already knowing what the
   ID is")


(defun get-circularity-vector ()
  "Get a fresh vector"
  (or (with-mutex (*serializer-lock*)
        (and (plusp (length *circularity-vector-queue*))
             (vector-pop *circularity-vector-queue*)))
      (make-array 50 :element-type t :initial-element nil 
                     :fill-pointer 0 :adjustable t)))

(defun release-circularity-vector (vector)
  "Don't need to erase, just reset fill-pointer as it 
   determines extent of valid data"
  (setf (fill-pointer vector) 0)
  (with-mutex (*serializer-lock*)
    (vector-push-extend vector *circularity-vector-queue* 20)))

;;
;; SERIALIZER
;;

(defconstant +2^31+ (expt 2 31))
(defconstant +2^32+ (expt 2 32))
(defconstant +2^63+ (expt 2 63))
(defconstant +2^64+ (expt 2 64))

(defun serialize-string (string bstream)
  "Try to write each format type and bail if code is too big"
  (declare (type buffer-stream bstream)
           (type string string))
  (cond ((and (not (equal "" string)) (> (char-code (char string 0)) #xFFFF))
         (serialize-to-utf32le string bstream))
        ;; Accelerate the common case where a character set is not Latin-1
        ((and (not (equal "" string)) (> (char-code (char string 0)) #xFF))
         (or (serialize-to-utf16le string bstream)
             (serialize-to-utf32le string bstream)))
        ;; Actually code pages > 0 are rare; so we can pay an extra cost
        (t (or (serialize-to-utf8 string bstream)
               (serialize-to-utf16le string bstream)
               (serialize-to-utf32le string bstream)))))

(defun serialize-to-utf8 (string bstream)
  "Standard serialization"
  (declare (type stream bstream)
           (type string string))
  ;; TODO
  (with-slots (size (allocated len) buffer) bstream
    (let* ((saved-size (the fixnum (size bstream)))
           (saved-pos (the fixnum (file-position bstream)))
           (characters (the fixnum (length string))))
      (labels ((fail () 
                 (setf (size bstream) saved-size)
                 (file-position bstream saved-pos)
                 (return-from serialize-to-utf8 nil))
               (succeed ()
                 (return-from serialize-to-utf8 t)))
        (write-byte +utf8-string+ bstream)
        (write-int32 characters bstream)
        (let ((needed (the fixnum (+ size characters))))
          (declare (type fixnum needed))
          (when (the boolean (> needed allocated))
            (resize-buffer-stream bstream needed))
          (etypecase string
            (simple-string
             (loop for i fixnum from 0 below characters do
                      (let ((code (the fixnum 
                                       (char-code 
                                        (the character (schar string i))))))
                        (declare (type fixnum code))
                        (when (the boolean (> code #xFF)) (fail))
                        (setf (aref buffer (the fixnum (+ i size))) code))))
            (string
             (loop for i fixnum from 0 below characters do 
                      (let ((code (the fixnum
                                       (char-code 
                                        (the character (char string i))))))
                        (declare (type fixnum code))
                        (when (> code #xFF) (fail))
                        (setf (aref buffer (the fixnum (+ i size))) code)))))
          (setf (size bstream) needed)
          (succeed))))))

(defun serialize-to-utf16le (string bstream)
  "Serialize to utf16le compliant format unless contains code pages > 0"
  (declare (type buffer-stream bstream)
           (type string string))
  (let ((buffer (buffer bstream))
        (size (size bstream))
        (allocated (len bstream)))
    (let* ((saved-size (size bstream))
           (saved-pos (file-position bstream))
           (characters (length string)))
      (labels ((fail () 
                 (setf (size bstream) saved-size)
                 (file-position bstream saved-pos)
                 (return-from serialize-to-utf16le nil))
               (succeed ()
                 (return-from serialize-to-utf16le t)))
        (write-byte +utf16-string+ bstream)
        (write-int32 characters bstream)
        (let ((needed (+ size (* characters 2)))
              (char (etypecase string
                      (simple-string #'schar)
                      (string #'char))))
          (when (> needed allocated)
            (resize-buffer-stream bstream needed))
          (loop for i fixnum from 0 below characters do
                   (let ((code (char-code (funcall char string i))))
                     (when (> code #xFFFF) (fail))
                     (setf (aref buffer (+ (* i 2) size))
                           ;;			  (coerce (ldb (byte 8 8) code) '(signed 8)))
                           (ldb (byte 8 8) code))
                     (setf (aref buffer (+ (* i 2) size 1))
                           ;;			  (coerce (ldb (byte 8 0) code) '(signed 8))))))
                           (ldb (byte 8 0) code))))
          (incf size (* characters 2))
          (succeed))))))

(defun serialize-to-utf32le (string bstream)
  "Serialize to utf32 compliant format unless contains code pages > 0"
  (declare (type buffer-stream bstream)
           (type string string))
  (with-slots (buffer size (allocated len)) bstream
    (let* ((characters (length string)))
      (write-byte +utf32-string+ bstream)
      (write-int32 characters bstream)
      (let ((needed (+ size (* 4 characters)))
            (char (etypecase string
                    (simple-string #'schar)
                    (string #'char))))
        (when (> needed allocated)
          (resize-buffer-stream bstream needed))
        (loop for i fixnum from 0 below characters do
                 (let ((code (char-code (funcall char string i))))
                   ;; (when (> code #x10FFFF) (error "Invalid unicode code type"))
                   (setf (aref buffer (+ (* i 4) size 0))
                         (ldb (byte 8 24) code))
                   (setf (aref buffer (+ (* i 4) size 1))
                         (ldb (byte 8 16) code))
                   (setf (aref buffer (+ (* i 4) size 2))
                         (ldb (byte 8 8) code))
                   (setf (aref buffer (+ (* i 4) size 3))
                         (ldb (byte 8 0) code)))))
      (incf size (* characters 4))
      t)))

(defun ser (frob bs sc)
  "Serialize a lisp value into a buffer-stream."
  (declare (type buffer-stream bs)
           (ignorable sc))
  (let ((lisp-obj-id -1)
        (circularity-hash 
          (unless (or (stringp frob) (symbolp frob) (numberp frob))
            (get-circularity-hash))))
    (declare (type fixnum lisp-obj-id))
    (labels 
        ((%next-object-id ()
           (incf lisp-obj-id))
         (%serialize (frob)
           ;;	   (format t "Serializing ~A of type ~A~%" frob (type-of frob))
           (typecase frob
             (fixnum 
              (if (< #.most-positive-fixnum +2^31+) ;; should be compiled away
                  (progn
                    (write-byte +fixnum32+ bs)
                    (write-fixnum32 frob bs))
                  (progn
                    (assert (eq (< #.most-positive-fixnum +2^63+) t))
                    (if (< (abs frob) +2^31+)
                        (progn
                          (write-byte +fixnum32+ bs)
                          (write-fixnum32 frob bs))
                        (progn
                          (write-byte +fixnum64+ bs)
                          (write-fixnum64 frob bs))))))
             (null
              (write-byte +nil+ bs))
             (symbol
              (let ((sym-name (symbol-name frob)))
                (declare (type string sym-name)
                         (dynamic-extent sym-name))
                (write-byte +symbol+ bs)
                (serialize-string sym-name bs)
                (let ((package (symbol-package frob)))
                  (declare (dynamic-extent package)
                           (type (or null package) package))
                  (if package
                      (serialize-string (package-name package) bs)
                      (write-byte +nil+ bs)))))
             ;;		(let ((package-name (gethash frob symbol-package-hash)))
             ;;		  (unless package-name
             ;;		    (setq package-name 
             ;;			  (setf (gethash frob symbol-package-hash)
             ;;				(package-name (symbol-package frob)))))
             ;;		  (if package-name
             ;;		      (serialize-string package-name bs)
             ;;		      (buffer-write-byte +nil+ bs)))))
             (string
              (serialize-string frob bs))
             (stored
              ;; TODO
              ;; (unless (valid-stored-reference-p frob sc)
              ;;   (cross-reference-error frob sc))
              ;; (when (store-marking-p sc)
              ;;   (gc-mark-new-write frob))
              (write-byte +stored-ref+ bs)
              (write-oid (oid frob) bs))
             #+lispworks
             (short-float
              (buffer-write-byte +short-float+ bs)
              (buffer-write-float (coerce frob 'single-float) bs))
             (single-float
              (write-byte +single-float+ bs)
              (write-float frob bs))
             (double-float
              (write-byte +double-float+ bs)
              (write-double frob bs))
             (standard-object
              ;; NOTE: Add support for schema validation
              (write-byte +object+ bs)
              (let ((idp (gethash frob circularity-hash)))
                (if idp (write-int32 idp bs)
                    (progn
                      (let ((id (%next-object-id)))
                        (write-int32 id bs)
                        (setf (gethash frob circularity-hash) id))
                      (%serialize (type-of frob))
                      (let ((svs (slots-and-values frob)))
                        (%serialize (/ (length svs) 2))
                        (loop for item in svs
                              do (%serialize item)))))))
             (integer
              (serialize-bignum frob bs))
             (rational
              (write-byte +rational+ bs)
              (%serialize (numerator frob))
              (%serialize (denominator frob)))
             (character
              (write-byte +char+ bs)
              ;; might be wide!
              (write-uint32 (char-code frob) bs))
             ;;	     (oid-pair
             ;;	      (buffer-write-byte +oid-pair+ bs)
             ;;	      (buffer-write-int32 (oid-pair-left frob) bs)
             ;;	      (buffer-write-int32 (oid-pair-right frob) bs))
             (cons
              (write-byte +cons+ bs)
              (let ((idp (gethash frob circularity-hash)))
                (if idp (write-int32 idp bs)
                    (progn
                      (let ((id (%next-object-id)))
                        (write-int32 id bs)
                        (setf (gethash frob circularity-hash) id))
                      (%serialize (car frob))
                      (%serialize (cdr frob))))))
             (pathname
              (let ((pstring (namestring frob)))
                (write-byte +pathname+ bs)
                (serialize-string pstring bs)))
             (complex 
              (write-byte +complex+ bs)
              (%serialize (realpart frob))
              (%serialize (imagpart frob)))
             (hash-table
              (write-byte +hash-table+ bs)
              (let ((idp (gethash frob circularity-hash)))
                (if idp (write-int32 idp bs)
                    (progn
                      (let ((id (%next-object-id)))
                        (write-int32 id bs)
                        (setf (gethash frob circularity-hash) id))
                      (%serialize (hash-table-test frob))
                      (%serialize (hash-table-rehash-size frob))
                      (%serialize (hash-table-rehash-threshold frob))
                      (%serialize (hash-table-count frob))
                      (loop for key being the hash-key of frob
                            using (hash-value value)
                            do 
                               (%serialize key)
                               (%serialize value))))))
             (array
              (write-byte +array+ bs)
              (let ((idp (gethash frob circularity-hash)))
                (if idp (write-int32 idp bs)
                    (progn
                      (let ((id (%next-object-id)))
                        (write-int32 id bs)
                        (setf (gethash frob circularity-hash) id))
                      (write-byte 
                       (logior (byte-from-array-type (array-element-type frob))
                               (if (array-has-fill-pointer-p frob) 
                                   +fill-pointer-p+ 0)
                               (if (adjustable-array-p frob) 
                                   +adjustable-p+ 0))
                       bs)
                      (let ((rank (array-rank frob)))
                        (write-int32 rank bs)
                        (loop for i fixnum from 0 below rank
                              do (%serialize (array-dimension frob i))))
                      (when (array-has-fill-pointer-p frob)
                        (%serialize (fill-pointer frob)))
                      (loop for i fixnum from 0 below (array-total-size frob)
                            do
                               (%serialize (row-major-aref frob i)))))))
             (structure-object 
              (write-byte +struct+ bs)
              (let ((idp (gethash frob circularity-hash)))
                (if idp (write-int32 idp bs)
                    (progn
                      (write-int32 (incf lisp-obj-id) bs)
                      (setf (gethash frob circularity-hash) lisp-obj-id)
                      (%serialize (type-of frob))
                      (let ((svs (struct-slots-and-values frob)))
                        (%serialize (/ (length svs) 2))
                        (loop for item in svs
                              do (%serialize item)))))))
             (t (format t "Can't serialize object: ~A of type ~A~%" frob (type-of frob))))))
      (%serialize frob)
      (when circularity-hash
        (release-circularity-hash circularity-hash))
      bs)))

(defun serialize-bignum (frob bs)
  "Serialize bignum to buffer stream"
  (declare (type integer frob)
           (type buffer-stream bs))
  (let* ((num (abs frob))
         (word-size (ceiling (/ (integer-length num) 32)))
         (needed (* word-size 4))
         (byte-spec (byte 32 0)))
    (declare (type fixnum word-size needed)
             (type cons byte-spec)
             (ignorable byte-spec))
    (if (< frob 0) 
        (write-byte +negative-bignum+ bs)
        (write-byte +positive-bignum+ bs))
    (write-uint32 needed bs)
    (loop for i fixnum from 0 below word-size 
          do (write-uint32 (ldb (byte 32 (* 32 i)) num) bs))))

;;;
;;; DESERIALIZER
;;;

(defparameter *trace-deserializer* t)

(defparameter *tag-table*
  `((,+fixnum32+ . "fixnum32")
    (,+fixnum64+ . "fixnum64")
    (,+char+ . "char")
    (,+short-float+ . "short-float")
    (,+single-float+ . "single-float")
    (,+double-float+ . "double float")
    (,+negative-bignum+ . "neg bignum")
    (,+positive-bignum+ . "pos bignum")
    (,+rational+ . "rational number")
    (,+nil+ . "null")
    (,+utf8-string+ . "UTF8 string")
    (,+utf16-string+ . "UTF16le string")
    (,+utf32-string+ . "UTF32le string")
    (,+symbol+ . "symbol")
    (,+pathname+ . "pathname")
    (,+persistent+ . "stored object (old)")
    (,+persistent-ref+ . "stored object reference (new)")
    ;;    (,+oid-pair+ . "oid pair for associations")
    (,+cons+ . "cons cell")
    (,+hash-table+ . "hash table")
    (,+object+ . "standard object")
    (,+array+ . "array")
    (,+struct+ . "struct")
    (,+class+ . "class")
    (,+complex+ . "complex")))

(defun enable-deserializer-tracing ()
  (setf *trace-deserializer* t))

(defun disable-deserializer-tracing ()
  (setf *trace-deserializer* nil))

(defun print-pre-deserialize-tag (tag)
  (when *trace-deserializer*
    (let ((tag-name (assoc tag *tag-table*)))
      (if tag-name
          (format t "Deserializing type: ~A~%" tag-name)
          (progn
            (format t "Unrecognized tag: ~A~%" tag)
            (break))))))

(defun print-post-deserialize-value (value)
  (when *trace-deserializer*
    (format t "Returned: ~A~%" value)))

;;
;; Deserialization of Strings 
;; 

(defparameter native-string-type
  #+(and allegro ics) :utf16le
  #+(and allegro (not ics)) :utf8
  #+(and sbcl sb-unicode) :utf32le
  #+(and sbcl (not sb-unicode)) :utf8
  #+lispworks :utf16le
  #+(and openmcl (not openmcl-unicode-strings)) :utf8
  #+openmcl-unicode-strings :utf32le
  )

(defun compatible-unicode-support-p (encoding-type)
  "This is a crude hack and can be improved later, but
   we assume if you have code pages > 0 you need or use
   a 32-bit encoding.  I'm assuming that 16-bit unicode
   supporting lisps only support code page 0 and do not
   use conjugate pair coding and variable length unicode
   string representations (formal utf-16)"
  (or (eq encoding-type :utf8) 
      (eq encoding-type native-string-type)
      (and (eq encoding-type :utf16le) (eq native-string-type :utf32le))))

(defgeneric deserialize-string (type bstream &optional temp-string))


#+lispworks 
(defmethod deserialize-string :around ((type t) bstream &optional temp-string)
  (coerce (call-next-method) 'lispworks:simple-text-string))

;; #+allegro
;; (defmethod deserialize-string ((type (eql :utf8)) bstream &optional temp-string)
;;   (declare (type buffer-stream bstream)
;; 	   (type string temp-string)
;; 	   (type symbol type))
;;   (let ((buffer buffer-stream-buffer)
;; 					(size buffer-stream-size)
;; 					(allocated buffer-stream-length))
;;       bstream
;;     (declare (type array-or-pointer-char buffer)
;; 	     (type fixnum size allocated))
;;     (let* ((length (the fixnum (read-int32 bstream)))
;; 	   (pos (the fixnum (file-position bstream))))
;;       (multiple-value-bind (string chars octets)
;; 	  (if temp-string
;; 	      (excl:native-to-string (+ buffer pos)
;; 				     :string temp-string :make-string? nil
;; 				     :length length)
;; 	      (excl:native-to-string (+ buffer pos) :length length))
;; 	(declare (ignorable chars))
;; 	(file-position bstream octets)
;; 	string))))

;; #-allegro
(defmethod deserialize-string ((type (eql :utf8)) bstream &optional temp-string)
  (declare (type buffer-stream bstream)
           (type (or null string) temp-string)
           (type symbol type))
  ;; Default char-code method
  (let* ((length (the fixnum (read-int32 bstream)))
         (pos (the fixnum (file-position bstream))))
    (file-position bstream length)
    (progn
      (let ((string (the string (or temp-string (make-string length :element-type 'character)))))
        (loop for i fixnum from 0 below length do
                 (setf (char string i)
                       (the character 
                            (code-char 
                             (the fixnum (aref (buffer bstream) 
                                               (+ pos i)))))))
        string))))

(defmethod deserialize-string ((type (eql :utf16le)) bstream &optional temp-string)
  "All returned strings are simple-strings for, uh, simplicity"
  (declare (type buffer-stream bstream))
  (let* ((length (read-int32 bstream))
         (string (or temp-string (make-string length :element-type 'character)))
         (pos (file-position bstream))
         (code 0))
    (macrolet ((next-byte (offset)
                 `(aref (buffer-stream-buffer bstream) (+ (* i 2) pos ,offset))))
      (declare (type simple-string string)
               (type fixnum length pos code))
      (assert (subtypep (type-of string) 'simple-string))
      (assert (compatible-unicode-support-p :utf16le))
      (loop for i fixnum from 0 below length do
               (setf code (dpb (next-byte 0) (byte 8 8) 0))
               (setf code (dpb (next-byte 1) (byte 8 0) code))
               (setf (schar string i) (code-char code)))
      (file-position bstream (* length 2)))
    (the simple-string string)))

(defmethod deserialize-string ((type (eql :utf32le)) bstream  &optional temp-string)
  (declare (type buffer-stream bstream))
  (macrolet ((next-byte (offset)
               `(aref (buffer-stream-buffer bstream) (+ (* i 4) pos ,offset))))
    (let* ((length (read-int32 bstream))
           (string (or temp-string (make-string length :element-type 'character)))
           (pos (file-position bstream))
           (code 0))
      (declare (type string string)
               (type fixnum length pos code))
      (assert (subtypep (type-of string) 'simple-string))
      (assert (compatible-unicode-support-p :utf32le))
      (loop for i fixnum from 0 below length do
               (setf code (dpb (next-byte 0) (byte 8 24) 0))
               (setf code (dpb (next-byte 1) (byte 8 16) code))
               (setf code (dpb (next-byte 2) (byte 8 8) code))
               (setf code (dpb (next-byte 3) (byte 8 0) code))
               (setf (char string i) (code-char code)))
      (file-position bstream (* length 4))
      (the simple-string string))))

(defun deser (buf-str sc &optional oid-only)
  "Deserialize a lisp value from a buffer-stream."
  (declare (type (or null buffer-stream) buf-str))
  (let ((circularity-vector (get-circularity-vector)))
    (labels 
        ((lookup-id (id)
           (if (>= id (fill-pointer circularity-vector)) nil
               (aref circularity-vector id)))
         (add-object (object)
           (vector-push-extend object circularity-vector 50)
           (1- (fill-pointer circularity-vector)))
         (%deserialize (bs)
           (declare (type buffer-stream bs))
           (let ((tag (read-byte bs)))
             (declare (type alien tag)
                      (dynamic-extent tag))
             ;;	   (print-pre-deserialize-tag tag)
             (let ((value  
                     (cond
                       ((= tag +fixnum32+)
                        (read-fixnum32 bs))
                       ((= tag +fixnum64+)
                        (read-fixnum64 bs))
                       ((= tag +nil+) nil)
                       ((= tag +utf8-string+)
                        #+lispworks
                        (coerce (deserialize-string :utf8 bs) 'base-string)

                        (deserialize-string :utf8 bs))
                       ((= tag +utf16-string+)
                        #+lispworks
                        (coerce (deserialize-string :utf16le bs) 'lw:text-string)
                        #-lispworks
                        (deserialize-string :utf16le bs))
                       ((= tag +utf32-string+)
                        #+lispworks
                        (coerce (deserialize-string :utf32le bs) 'sys:augmented-string)
                        #-lispworks
                        (deserialize-string :utf32le bs))
                       ((= tag +symbol+)
                        (let ((name (%deserialize bs))
                              (package (%deserialize bs)))
                          (translate-and-intern-symbol sc name package)))
                       ((= tag +stored+)
                        (let ((oid (read-oid bs))
                              (cname (%deserialize bs)))
                          (if oid-only oid
                              (store-recreate-instance sc oid cname))))
                       ((= tag +stored-ref+)
                        (let ((oid (read-oid bs)))
                          (if oid-only oid
                              (store-recreate-instance sc oid))))
                       #+lispworks
                       ((= tag +short-float+)
                        (coerce (read-float bs) 'short-float))
                       ((= tag +single-float+)
                        (read-float bs))
                       ((= tag +double-float+)
                        (read-double bs))
                       ((= tag +char+)
                        (code-char (read-uint32 bs)))
                       ((= tag +pathname+)
                        (parse-namestring (or (%deserialize bs) "")))
                       ((= tag +positive-bignum+) 
                        (deserialize-bignum bs (read-uint32 bs) t))
                       ((= tag +negative-bignum+) 
                        (deserialize-bignum bs (read-uint32 bs) nil))
                       ((= tag +rational+) 
                        (/ (the integer (%deserialize bs)) 
                           (the integer (%deserialize bs))))
                       ;;	     ((= tag +oid-pair+)
                       ;;	      (let ((pair (make-oid-pair)))
                       ;;		(setf (oid-pair-left pair) (read-fixnum32 bs))
                       ;;		(setf (oid-pair-right pair) (read-fixnum32 bs))))
                       ((= tag +cons+)
                        (let* ((id (read-int32 bs))
                               (maybe-cons (lookup-id id)))
                          (declare (type fixnum id))
                          (if maybe-cons maybe-cons
                              (let ((c (cons nil nil)))
                                (add-object c)
                                (setf (car c) (%deserialize bs))
                                (setf (cdr c) (%deserialize bs))
                                c))))
                       ((= tag +complex+)
                        (let ((rpart (%deserialize bs))
                              (ipart (%deserialize bs)))
                          (complex rpart ipart)))
                       ((= tag +hash-table+)
                        (let* ((id (read-int32 bs))
                               (maybe-hash (lookup-id id)))
                          (declare (type fixnum id))
                          ;;		(format t "~A ~A~%" maybe-hash id)
                          (if maybe-hash maybe-hash
                              (let* ((test (%deserialize bs))
                                     (rehash-size (%deserialize bs))
                                     (rehash-threshold (%deserialize bs))
                                     (size (%deserialize bs))
                                     (h (make-hash-table :test test
                                                         :rehash-size rehash-size
                                                         :rehash-threshold rehash-threshold
                                                         :size (ceiling (* (ceiling (/ (+ size 10) rehash-threshold)) rehash-size)))))
                                (add-object h)
                                (loop for i fixnum from 0 below size
                                      do
                                         (setf (gethash (%deserialize bs) h)
                                               (%deserialize bs)))
                                h))))
                       ((= tag +object+)
                        (let* ((id (read-int32 bs))
                               (maybe-o (lookup-id id)))
                          (if maybe-o maybe-o
                              (let ((typedesig (%deserialize bs)))
                                ;; now, depending on what typedesig is, we might 
                                ;; or might not need to specify the store controller here..
                                (let ((o 
                                        (or (handler-case
                                                (if (subtypep typedesig 'stored)
                                                    (recreate-instance-using-class (find-class typedesig) :store sc)
                                                    ;; if the this type doesn't exist in our object
                                                    ;; space, we can't reconstitute it, but we don't want 
                                                    ;; to abort completely, we will return a special object...
                                                    ;; This behavior could be configurable; the user might 
                                                    ;; prefer an abort here, but I prefer surviving...
                                                    (make-instance typedesig))
                                              (error (v) (format t "got typedesig error: ~A ~A ~%" v typedesig)
                                                (list 'caught-error v typedesig)))
                                            (list 'uninstantiable-object-of-type typedesig))))
                                  (if (listp o)
                                      o
                                      (progn
                                        (add-object o)
                                        (loop for i fixnum from 0 below (%deserialize bs)
                                              do
                                                 (setf (slot-value o (%deserialize bs))
                                                       (%deserialize bs)))
                                        o)))))))
                       ((= tag +array+)
                        (let* ((id (read-int32 bs))
                               (maybe-array (lookup-id id)))
                          (if maybe-array maybe-array
                              (let* ((flags (read-byte bs))
                                     (a (make-array 
                                         (loop for i fixnum from 0 below 
                                                  (read-int32 bs)
                                               collect (%deserialize bs))
                                         :element-type (array-type-from-byte 
                                                        (logand #x1f flags))
                                         :fill-pointer (/= 0 (logand +fill-pointer-p+ 
                                                                     flags))
                                         :adjustable (/= 0 (logand +adjustable-p+ 
                                                                   flags)))))
                                (when (array-has-fill-pointer-p a)
                                  (setf (fill-pointer a) (%deserialize bs)))
                                (add-object a)
                                (loop for i fixnum from 0 below (array-total-size a)
                                      do
                                         (setf (row-major-aref a i) (%deserialize bs)))
                                a))))
                       ((= tag +struct+)
                        (let* ((id (read-int32 bs))
                               (maybe-o (lookup-id id)))
                          (if maybe-o maybe-o
                              (let ((typedesig (%deserialize bs)))
                                (let ((o (or (handler-case
                                                 (funcall (struct-constructor typedesig))
                                               (error (v) (format t "got typedesig error for struct: ~A ~A ~%" v typedesig)
                                                 (list 'caught-error v typedesig)))
                                             (list 'uninstantiable-object-of-type typedesig))))
                                  (if (listp o) o
                                      (progn
                                        (add-object o)
                                        (loop for i fixnum from 0 below (%deserialize bs) do
                                                 (let ((name (%deserialize bs))
                                                       (value (%deserialize bs)))
                                                   (setf (slot-value o name) value)))
                                        o)))))))
                       (t (error 'elephant-type-deserialization-error :type-tag tag)))))
               ;;	     (print-post-deserialize-value value)
               value))))
      (etypecase buf-str 
        (null (return-from deser nil))
        (buffer-stream
         (let ((result (%deserialize buf-str)))
           (release-circularity-vector circularity-vector)
           result))))))

(defun deserialize-bignum (bs length positive)
  (declare (type buffer-stream bs)
           (type fixnum length)
           (type boolean positive))
  (let ((int-byte-spec (byte 32 0)))
    (declare (dynamic-extent int-byte-spec)
             (ignorable int-byte-spec))
    (loop for i from 0 below (/ length 4)
          for byte-spec = 
          ;;	 #+(or allegro) (progn (setf (cdr int-byte-spec) (* 32 i)) int-byte-spec)
             #+(or allegro sbcl cmu lispworks openmcl) (byte 32 (* 32 i))
          with num of-type integer = 0 
          do
             (setq num (dpb (read-uint32 bs) byte-spec num))
          finally 
             (return (if positive num (- num))))))
