;;; tcompact.lisp --- Thrift Compact Protocol

;; ref: https://github.com/apache/thrift/blob/master/doc/specs/thrift-compact-protocol.md

;;; Commentary:

;; in order to encode Parquet, we need to be able to encode the Thrift Compact
;; Protocol (TCompact). All thrift structures we've generated via parquet.json
;; are serialized using TCompact.

;; see also: https://thrift.apache.org/static/files/thrift-20070401.pdf

;;; Code:
(in-package :dat/parquet)

;;; Protocol

(defclass thrift-object (id) ())

(defgeneric thrift-element-type (self)
  (:method ((self parquet-struct-object)) :struct))

(defgeneric thrift-object-length (self))

;;; Integers

#|
50399 =          11000100 11011111  (LSB)
      =  0000011  0001001  1011111  (7-bit groups)
      = 00000011 10001001 11011111  (add continuation bits)
      =     0x03     0x89     0xDF  (hex)
→ 0xDF 0x89 0x03 (write to ram LSB first)
|#

;; encoded as ULEB128. signed and unsigned bytes are encoded as single
;; bytes. all others are coverted to int64.
(defun zigzag (n)
  (declare (integer n))
  (logxor (ash n 1) (ash n -63)))

(defun zagzig (n)
  (declare (integer n))
  (logxor (ash n -1) (- (logand n 1))))

(defun tcompact-encode-integer (n &optional (size 8))
  (declare (integer n))
  (if (<= (integer-length n) 8)
      (vector n)
      (encode-uleb128 (zigzag n) size)))

;;; Enums

;; ordinal value encoded as int32
(defun tcompact-encode-enum (n)
  (tcompact-encode-integer n 4))

;;; Binary

#|
Binary protocol, binary data, 1+ bytes:
+--------+...+--------+--------+...+--------+
| byte length         | bytes               |
+--------+...+--------+--------+...+--------+
|#

;; a varint followed by the bytes
(defun tcompact-encode-octet-vector (octets)
  (concatenate 'octet-vector
               (tcompact-encode-integer (length octets))
               octets))

;;; String

;; encoded as UTF-8 bytes without null-termination
(defun tcompact-encode-string (string)
  (sb-ext:string-to-octets string :external-format :utf-8))

;;; Double
(defun tcompact-encode-double (float)
  (tcompact-encode-integer (encode-float32 float)))

;;; Boolean

(defun tcompact-encode-boolean (bool)
  (if bool 1 0))

;;; UUID

;; always 16 bytes, no length header
(defun tcompact-encode-uuid (uuid)
  (declare (obj/uuid:uuid uuid))
  (obj/uuid:uuid-to-octet-vector uuid))

;;; Structs

;; struct        ::= ( field-header field-value )* stop-field
;; field-header  ::= field-type field-id

#|
Compact protocol field header (short form) and field value:
+--------+--------+...+--------+
|ddddtttt| field value         |
+--------+--------+...+--------+

Compact protocol field header (1 to 3 bytes, long form) and field value:
+--------+--------+...+--------+--------+...+--------+
|0000tttt| field id            | field value         |
+--------+--------+...+--------+--------+...+--------+

Compact protocol stop field:
+--------+
|00000000|
+--------+
|#

;; sequences of zero or more 'fields' followed by a stop field.

;; each field starts with a field header and is followed by the encoded field
;; value.

;; the field-id is represented in Lisp via OBJ/ID.

;; note that it is possible to handle unknown fields while decoding. in the
;; usual case these are ignored.

(declaim ((unsigned-byte 8) +tcompact-stop-field+))
(defconstant +tcompact-stop-field+ 0)
(deftype tcompact-field-id () '(integer 0 32767))
(deftype tcompact-field-id-delta () '(unsigned-byte 4))
(deftype tcompact-field-type-id () '(unsigned-byte 4))

(defvar *tcompact-field-types*
  #(:true :false :i8 :i16 :i32 :i64 :double :binary :list :set :map :struct :uuid))
(defun tcompact-field-type-id* (n) (1+ (aref *tcompact-field-types* n)))
(defun tcompact-field-type-id (k) (1+ (position k *tcompact-field-types*)))

;; (ldb (byte 4 0) n)
(defun tcompact-encode-field-header-short (id-delta type-id)
  (dpb type-id (byte 4 4)
       (dpb id-delta (byte 4 0) 0)))

(defun tcompact-encode-field-id (id)
  (tcompact-encode-integer id))

(defun tcompact-encode-field-header (field)
  (let ((ret (make-array 5 :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (vector-push (tcompact-encode-field-header-short 0 (tcompact-field-type-id* field))
                 ret)
    (loop for x across (tcompact-encode-field-id (id field))
          do (vector-push x ret)
          finally (return ret))))

(defun tcompact-encode-field-value (field))

(defun tcompact-encode-struct (struct))

  ;; field-id-delta = current-field-id - previous-field-id

;;; List and Set

#|
Compact protocol list header (1 byte, short form) and elements:
+--------+--------+...+--------+
|sssstttt| elements            |
+--------+--------+...+--------+

Compact protocol list header (2+ bytes, long form) and elements:
+--------+--------+...+--------+--------+...+--------+
|1111tttt| size                | elements            |
+--------+--------+...+--------+--------+...+--------+
|#

(deftype tcompact-element-type-id () '(unsigned-byte 4))
;; tcompact short size = [0,14]

(defvar *tcompact-element-types*
  #(:bool :i8 :i16 :i32 :i64 :double :binary :list :set :map :struct :uuid))

(defun tcompact-element-type-id* (n) (+ (aref *tcompact-element-types* n) 2))
(defun tcompact-element-type-id (k) (+ (position k *tcompact-element-types*) 2))

(defun tcompact-encode-list-header-short (size elt-type)
  (dpb elt-type (byte 4 4)
       (dpb size (byte 4 0) 0)))

(defun tcompact-encode-list-header (list)
  (let ((ret (make-array 5 :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (vector-push (tcompact-encode-list-header-short #xf (id list)) ret)
    (loop for x across (tcompact-encode-integer (thrift-object-length list) 4)
          do (vector-push x ret)
          finally (return ret))))

(defun tcompact-encode-list-element (type value))

;;; Map

;; map           ::= empty-map | non-empty-map
;; empty-map     ::= `0`
;; non-empty-map ::= size key-element-type value-element-type (key value)+

#|
Compact protocol map header (1 byte, empty map):
+--------+
|00000000|
+--------+

Compact protocol map header (2+ bytes, non empty map) and key value pairs:
+--------+...+--------+--------+--------+...+--------+
| size                |kkkkvvvv| key value pairs     |
+--------+...+--------+--------+--------+...+--------+
|#

(defun tcompact-encode-map ())
