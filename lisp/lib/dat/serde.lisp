;;; serde.lisp --- Binary Lisp Data Formats

;; Direct De/Serialization of Lisp Objects to/from OCTET vectors and streams.

;;; Code:
(in-package :dat/serde)
(declaim  (optimize speed))

(defvar *no-deserialization-package-found-action* :warn)

(defun translate-and-intern-symbol (symbol-name package-name)
  "Service for the serializer to translate any renamed packages or symbols
   and then intern the decoded symbol."
  (if package-name
      (if-let ((package (find-package package-name)))
        (intern symbol-name package)
        (intern symbol-name 
                (make-package package-name :use '(cl std obj))))
      (make-symbol symbol-name)))

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

;; Implementation-dependent types (see sr/compiler/generic/vm-type.lisp)
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

;;; SERIALIZER
(defconstant +2^31+ (expt 2 31))
(defconstant +2^32+ (expt 2 32))
(defconstant +2^63+ (expt 2 63))
(defconstant +2^64+ (expt 2 64))

(defvar *default-serde-buffer* #.(make-array 10 :element-type 'octet :adjustable t))

(defun serialize-string (string &optional (buf *default-serde-buffer*))
  "Try to write each format type and bail if code is too big"
  (declare (type octet-vector buf)
           (type string string))
  (cond ((and (not (equal "" string)) (> (char-code (char string 0)) #xFFFF))
         (serialize-to-utf32le string buf))
        ;; Accelerate the common case where a character set is not Latin-1
        ((and (not (equal "" string)) (> (char-code (char string 0)) #xFF))
         (or (serialize-to-utf16le string buf)
             (serialize-to-utf32le string buf)))
        ;; Actually code pages > 0 are rare; so we can pay an extra cost
        (t (or (serialize-to-utf8 string buf)
               (serialize-to-utf16le string buf)
               (serialize-to-utf32le string buf)))))

(defun serialize-to-utf8 (string &optional (buf *default-serde-buffer*))
  "Standard serialization"
  (declare (type static-stream buf)
           (type string string))
  ;; TODO
  (with-slots (size (allocated len) buffer) buf
    (let* ((size (the fixnum (length buf)))
           (pos (the fixnum (file-position buf)))
           (chars (the fixnum (length string))))
      (labels ((fail () 
                 (file-position buf pos)
                 (return-from serialize-to-utf8 nil))
               (succeed ()
                 (return-from serialize-to-utf8 t)))
        (write-byte +utf8-string+ buf)
        (write-int32 chars buf)
        (let ((needed (the fixnum (+ size chars))))
          (declare (type fixnum needed))
          (when (the boolean (> needed allocated))
            (resize-static-stream buf needed))
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
          ;; (setf (size buf) needed)
          (succeed))))))

(defun serialize-to-utf16le (string &optional (buf *default-serde-buffer*))
  "Serialize to utf16le compliant format unless contains code pages > 0"
  (declare (type static-stream buf)
           (type string string))
  (let* ((buffer (buffer buf))
         (size (length buffer))
         (allocated (offset buf))
         (pos (file-position buf))
         (chars (length string)))
    (labels ((fail () 
                 (file-position buf pos)
                 (return-from serialize-to-utf16le nil))
               (succeed ()
                 (return-from serialize-to-utf16le t)))
        (write-byte +utf16-string+ buf)
        (write-int32 chars buf)
        (let ((needed (+ size (* chars 2)))
              (char (etypecase string
                      (simple-string #'schar)
                      (string #'char))))
          (when (> needed allocated)
            (resize-static-stream buf needed))
          (loop for i fixnum from 0 below chars do
                   (let ((code (char-code (funcall char string i))))
                     (when (> code #xFFFF) (fail))
                     (setf (aref buffer (+ (* i 2) size))
                           ;;			  (coerce (ldb (byte 8 8) code) '(signed 8)))
                           (ldb (byte 8 8) code))
                     (setf (aref buffer (+ (* i 2) size 1))
                           ;;			  (coerce (ldb (byte 8 0) code) '(signed 8))))))
                           (ldb (byte 8 0) code))))
          (incf size (* chars 2))
          (succeed)))))

(defun serialize-to-utf32le (string &optional (buf *default-serde-buffer*))
  "Serialize to utf32 compliant format unless contains code pages > 0"
  (declare (type octet-vector buf)
           (type string string))
  (with-slots (buffer size (allocated len)) buf
    (let* ((chars (length string)))
      (write-byte +utf32-string+ buf)
      (write-int32 chars buf)
      (let ((needed (+ size (* 4 chars)))
            (char (etypecase string
                    (simple-string #'schar)
                    (string #'char))))
        (when (> needed allocated)
          (resize-static-stream buf needed))
        (loop for i fixnum from 0 below chars do
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
      (incf size (* chars 4))
      t)))

(defun ser (frob buf store)
  "Serialize a lisp value into a static-stream."
  (declare (static-stream buf)
           (ignorable store))
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
                    (write-byte +fixnum32+ buf)
                    (write-fixnum32 frob buf))
                  (progn
                    (assert (eq (< #.most-positive-fixnum +2^63+) t))
                    (if (< (abs frob) +2^31+)
                        (progn
                          (write-byte +fixnum32+ buf)
                          (write-fixnum32 frob buf))
                        (progn
                          (write-byte +fixnum64+ buf)
                          (write-fixnum64 frob buf))))))
             (null
              (write-byte +nil+ buf))
             (symbol
              (let ((sym-name (symbol-name frob)))
                (declare (type string sym-name)
                         (dynamic-extent sym-name))
                (write-byte +symbol+ buf)
                (serialize-string sym-name buf)
                (let ((package (symbol-package frob)))
                  (declare (dynamic-extent package)
                           (type (or null package) package))
                  (if package
                      (serialize-string (package-name package) buf)
                      (write-byte +nil+ buf)))))
             ;;		(let ((package-name (gethash frob symbol-package-hash)))
             ;;		  (unless package-name
             ;;		    (setq package-name 
             ;;			  (setf (gethash frob symbol-package-hash)
             ;;				(package-name (symbol-package frob)))))
             ;;		  (if package-name
             ;;		      (serialize-string package-name buf)
             ;;		      (buffer-write-byte +nil+ buf)))))
             (string
              (serialize-string frob buf))
             (stored
              ;; TODO
              (unless (valid-stored-reference-p frob store)
                (signal-cross-store-error frob store))
              ;; (when (store-marking-p store)
              ;;   (gc-mark-new-write frob))
              (write-byte +stored-ref+ buf)
              (write-oid (oid frob) buf))
             (single-float
              (write-byte +single-float+ buf)
              (write-float frob buf))
             (double-float
              (write-byte +double-float+ buf)
              (write-double frob buf))
             (standard-object
              ;; NOTE: Add support for schema validation
              (write-byte +object+ buf)
              (let ((idp (gethash frob circularity-hash)))
                (if idp (write-int32 idp buf)
                    (progn
                      (let ((id (%next-object-id)))
                        (write-int32 id buf)
                        (setf (gethash frob circularity-hash) id))
                      (%serialize (type-of frob))
                      (let ((svs (slots-and-values frob)))
                        (%serialize (/ (length svs) 2))
                        (loop for item in svs
                              do (%serialize item)))))))
             (integer
              (serialize-bignum frob buf))
             (rational
              (write-byte +rational+ buf)
              (%serialize (numerator frob))
              (%serialize (denominator frob)))
             (character
              (write-byte +char+ buf)
              ;; might be wide!
              (write-uint32 (char-code frob) buf))
             ;;	     (oid-pair
             ;;	      (buffer-write-byte +oid-pair+ buf)
             ;;	      (buffer-write-int32 (oid-pair-left frob) buf)
             ;;	      (buffer-write-int32 (oid-pair-right frob) buf))
             (cons
              (write-byte +cons+ buf)
              (let ((idp (gethash frob circularity-hash)))
                (if idp (write-int32 idp buf)
                    (progn
                      (let ((id (%next-object-id)))
                        (write-int32 id buf)
                        (setf (gethash frob circularity-hash) id))
                      (%serialize (car frob))
                      (%serialize (cdr frob))))))
             (pathname
              (let ((pstring (namestring frob)))
                (write-byte +pathname+ buf)
                (serialize-string pstring buf)))
             (complex 
              (write-byte +complex+ buf)
              (%serialize (realpart frob))
              (%serialize (imagpart frob)))
             (hash-table
              (write-byte +hash-table+ buf)
              (let ((idp (gethash frob circularity-hash)))
                (if idp (write-int32 idp buf)
                    (progn
                      (let ((id (%next-object-id)))
                        (write-int32 id buf)
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
              (write-byte +array+ buf)
              (let ((idp (gethash frob circularity-hash)))
                (if idp (write-int32 idp buf)
                    (progn
                      (let ((id (%next-object-id)))
                        (write-int32 id buf)
                        (setf (gethash frob circularity-hash) id))
                      (write-byte 
                       (logior (byte-from-array-type (array-element-type frob))
                               (if (array-has-fill-pointer-p frob) 
                                   +fill-pointer-p+ 0)
                               (if (adjustable-array-p frob) 
                                   +adjustable-p+ 0))
                       buf)
                      (let ((rank (array-rank frob)))
                        (write-int32 rank buf)
                        (loop for i fixnum from 0 below rank
                              do (%serialize (array-dimension frob i))))
                      (when (array-has-fill-pointer-p frob)
                        (%serialize (fill-pointer frob)))
                      (loop for i fixnum from 0 below (array-total-size frob)
                            do
                               (%serialize (row-major-aref frob i)))))))
             (structure-object 
              (write-byte +struct+ buf)
              (let ((idp (gethash frob circularity-hash)))
                (if idp (write-int32 idp buf)
                    (progn
                      (write-int32 (incf lisp-obj-id) buf)
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
      buf)))

(defun serialize-bignum (frob buf)
  "Serialize bignum to buffer stream"
  (declare (integer frob)
           (static-stream buf))
  (let* ((num (abs frob))
         (word-size (ceiling (/ (integer-length num) 32)))
         (needed (* word-size 4))
         (byte-spec (byte 32 0)))
    (declare (type fixnum word-size needed)
             (type cons byte-spec)
             (ignorable byte-spec))
    (if (< frob 0) 
        (write-byte +negative-bignum+ buf)
        (write-byte +positive-bignum+ buf))
    (write-uint32 needed buf)
    (loop for i fixnum from 0 below word-size 
          do (write-uint32 (ldb (byte 32 (* 32 i)) num) buf))))

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
    (,+stored+ . "stored object (old)")
    (,+stored-ref+ . "stored object reference (new)")
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

(defparameter *native-string-type* :utf32le)

(defun compatible-unicode-support-p (encoding-type)
  "This is a crude hack and can be improved later, but
   we assume if you have code pages > 0 you need or use
   a 32-bit encoding.  I'm assuming that 16-bit unicode
   supporting lisps only support code page 0 and do not
   use conjugate pair coding and variable length unicode
   string representations (formal utf-16)"
  (or (eq encoding-type :utf8) 
      (eq encoding-type *native-string-type*)
      (and (eq encoding-type :utf16le) (eq *native-string-type* :utf32le))))

(defgeneric deserialize-string (type buffer &optional temp-string))

(defmethod deserialize-string ((type (eql :utf8)) buf &optional temp-string)
  (declare (type octet-vector buf)
           (type (or null string) temp-string)
           (type symbol type))
  ;; Default char-code method
  (let ((length (the fixnum (std/alien::read-alien-signed-byte-32 (sap-alien (static-vector-pointer buf) (* unsigned-char))))))
    (progn
      (let ((string (the string (or temp-string (make-string length :element-type 'character)))))
        (loop for i fixnum from 0 below length do
                 (setf (char string i)
                       (the character 
                            (code-char 
                             (the fixnum (aref buf i))))))
        string))))

(defmethod deserialize-string ((type (eql :utf16le)) buf &optional temp-string)
  "All returned strings are simple-strings for, uh, simplicity"
  (let* ((length (std/alien::read-alien-signed-byte-32 buf))
         (string (or temp-string (make-string length :element-type 'character)))
         (pos 0)
         (code 0))
    (macrolet ((next-byte (offset)
                 `(aref buf (+ (* i 2) pos ,offset))))
      (declare (type simple-string string)
               (type fixnum length pos code))
      (assert (subtypep (type-of string) 'simple-string))
      (assert (compatible-unicode-support-p :utf16le))
      (loop for i fixnum from 0 below length do
               (setf code (dpb (next-byte 0) (byte 8 8) 0))
               (setf code (dpb (next-byte 1) (byte 8 0) code))
               (setf (schar string i) (code-char code)))
      (setf pos (* length 2)))
    (the simple-string string)))

(defmethod deserialize-string ((type (eql :utf32le)) buf &optional temp-string)
  (macrolet ((next-byte (offset)
               `(aref buf (+ (* i 4) pos ,offset))))
    (let* ((length (std/alien::read-alien-signed-byte-32 buf))
           (string (or temp-string (make-string length :element-type 'character)))
           (pos 0)
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
      (setf pos (* length 4))
      (the simple-string string))))

(defun de (buf sc &optional oid-only)
  "Deserialize a lisp value from a static-stream."
  (declare ((or null static-stream) buf))
  (let ((circularity-vector (get-circularity-vector)))
    (labels 
        ((lookup-id (id)
           (if (>= id (fill-pointer circularity-vector)) nil
               (aref circularity-vector id)))
         (add-object (object)
           (vector-push-extend object circularity-vector 50)
           (1- (fill-pointer circularity-vector)))
         (%deserialize (buf)
           (declare (static-stream buf))
           (let ((tag (read-byte buf)))
             (declare (type alien tag)
                      (dynamic-extent tag))
             ;;	   (print-pre-deserialize-tag tag)
             (let ((value  
                     (cond
                       ((= tag +fixnum32+)
                        (read-fixnum32 buf))
                       ((= tag +fixnum64+)
                        (read-fixnum64 buf))
                       ((= tag +nil+) nil)
                       ((= tag +utf8-string+)
                        (deserialize-string :utf8 buf))
                       ((= tag +utf16-string+)
                        (deserialize-string :utf16le buf))
                       ((= tag +utf32-string+)
                        (deserialize-string :utf32le buf))
                       ((= tag +symbol+)
                        (let ((name (%deserialize buf))
                              (package (%deserialize buf)))
                          (translate-and-intern-symbol name package)))
                       ((= tag +stored+)
                        (let ((oid (read-oid buf))
                              (cname (%deserialize buf)))
                          (if oid-only oid
                              (store-recreate-instance sc oid cname))))
                       ((= tag +stored-ref+)
                        (let ((oid (read-oid buf)))
                          (if oid-only oid
                              (store-recreate-instance sc oid))))
                       ((= tag +single-float+)
                        (read-float buf))
                       ((= tag +double-float+)
                        (read-double buf))
                       ((= tag +char+)
                        (code-char (read-uint32 buf)))
                       ((= tag +pathname+)
                        (parse-namestring (or (%deserialize buf) "")))
                       ((= tag +positive-bignum+) 
                        (deserialize-bignum buf (read-uint32 buf) t))
                       ((= tag +negative-bignum+) 
                        (deserialize-bignum buf (read-uint32 buf) nil))
                       ((= tag +rational+) 
                        (/ (the integer (%deserialize buf)) 
                           (the integer (%deserialize buf))))
                       ;;	     ((= tag +oid-pair+)
                       ;;	      (let ((pair (make-oid-pair)))
                       ;;		(setf (oid-pair-left pair) (read-fixnum32 buf))
                       ;;		(setf (oid-pair-right pair) (read-fixnum32 buf))))
                       ((= tag +cons+)
                        (let* ((id (read-int32 buf))
                               (maybe-cons (lookup-id id)))
                          (declare (type fixnum id))
                          (if maybe-cons maybe-cons
                              (let ((c (cons nil nil)))
                                (add-object c)
                                (setf (car c) (%deserialize buf))
                                (setf (cdr c) (%deserialize buf))
                                c))))
                       ((= tag +complex+)
                        (let ((rpart (%deserialize buf))
                              (ipart (%deserialize buf)))
                          (complex rpart ipart)))
                       ((= tag +hash-table+)
                        (let* ((id (read-int32 buf))
                               (maybe-hash (lookup-id id)))
                          (declare (type fixnum id))
                          ;;		(format t "~A ~A~%" maybe-hash id)
                          (if maybe-hash maybe-hash
                              (let* ((test (%deserialize buf))
                                     (rehash-size (%deserialize buf))
                                     (rehash-threshold (%deserialize buf))
                                     (size (%deserialize buf))
                                     (h (make-hash-table :test test
                                                         :rehash-size rehash-size
                                                         :rehash-threshold rehash-threshold
                                                         :size (ceiling (* (ceiling (/ (+ size 10) rehash-threshold)) rehash-size)))))
                                (add-object h)
                                (loop for i fixnum from 0 below size
                                      do
                                         (setf (gethash (%deserialize buf) h)
                                               (%deserialize buf)))
                                h))))
                       ((= tag +object+)
                        (let* ((id (read-int32 buf))
                               (maybe-o (lookup-id id)))
                          (if maybe-o maybe-o
                              (let ((typedesig (%deserialize buf)))
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
                                        (loop for i fixnum from 0 below (%deserialize buf)
                                              do
                                                 (setf (slot-value o (%deserialize buf))
                                                       (%deserialize buf)))
                                        o)))))))
                       ((= tag +array+)
                        (let* ((id (read-int32 buf))
                               (maybe-array (lookup-id id)))
                          (if maybe-array maybe-array
                              (let* ((flags (read-byte buf))
                                     (a (make-array 
                                         (loop for i fixnum from 0 below 
                                                  (read-int32 buf)
                                               collect (%deserialize buf))
                                         :element-type (array-type-from-byte 
                                                        (logand #x1f flags))
                                         :fill-pointer (/= 0 (logand +fill-pointer-p+ 
                                                                     flags))
                                         :adjustable (/= 0 (logand +adjustable-p+ 
                                                                   flags)))))
                                (when (array-has-fill-pointer-p a)
                                  (setf (fill-pointer a) (%deserialize buf)))
                                (add-object a)
                                (loop for i fixnum from 0 below (array-total-size a)
                                      do
                                         (setf (row-major-aref a i) (%deserialize buf)))
                                a))))
                       ((= tag +struct+)
                        (let* ((id (read-int32 buf))
                               (maybe-o (lookup-id id)))
                          (if maybe-o maybe-o
                              (let ((typedesig (%deserialize buf)))
                                (let ((o (or (handler-case
                                                 (funcall (struct-constructor typedesig))
                                               (error (v) (format t "got typedesig error for struct: ~A ~A ~%" v typedesig)
                                                 (list 'caught-error v typedesig)))
                                             (list 'uninstantiable-object-of-type typedesig))))
                                  (if (listp o) o
                                      (progn
                                        (add-object o)
                                        (loop for i fixnum from 0 below (%deserialize buf) do
                                                 (let ((name (%deserialize buf))
                                                       (value (%deserialize buf)))
                                                   (setf (slot-value o name) value)))
                                        o)))))))
                       (t (error 'elephant-type-deserialization-error :type-tag tag)))))
               ;;	     (print-post-deserialize-value value)
               value))))
      (etypecase buf
        (null (return-from de nil))
        (staticr-stream
         (let ((result (%deserialize buf)))
           (release-circularity-vector circularity-vector)
           result))))))

(defun deserialize-bignum (buf length positive)
  (declare (type static-stream buf)
           (type fixnum length)
           (type boolean positive))
  (let ((int-byte-spec (byte 32 0)))
    (declare (dynamic-extent int-byte-spec)
             (ignorable int-byte-spec))
    (loop for i from 0 below (/ length 4)
          for byte-spec = (byte 32 (the fixnum (* 32 i)))
          with num of-type integer = 0 
          do (setq num (dpb (read-uint32 buf) byte-spec num))
          finally (return (if positive num (- num))))))
             
