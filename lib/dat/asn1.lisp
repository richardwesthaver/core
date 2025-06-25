;;; asn1.lisp --- Simple ASN.1 Coding

;; Abstract Syntax Notation One

;;; Commentary:

;; IDL for definiting data structures - joint standard between ITU-T and ISO
;; used to define a large number of protocols.

;; For example see the CRY/SSL/X509 package.

#| refs
 - https://github.com/digitalbazaar/forge/blob/909e312878838f46ba6d70e90264650b05eb8bde/js/asn1.js
 - http://www.obj-sys.com/asn1tutorial/node128.html
 - https://github.com/deadtrickster/ssl_verify_hostname.erl/blob/master/src/ssl_verify_hostname.erl
 - https://golang.org/src/encoding/asn1/asn1.go?m=text
|#

#|
 The most common binary encodings for ASN.1 are BER (Basic Encoding Rules)
 and DER (Distinguished Encoding Rules). DER is just a subset of BER that
 has stricter requirements for how data must be encoded.

 Each ASN.1 structure has a tag (a byte identifying the ASN.1 structure type)
 and a byte array for the value of this ASN1 structure which may be data or a
 list of ASN.1 structures.

 Each ASN.1 structure using BER is (Tag-Length-Value):

 | byte 0 | bytes X | bytes Y |
 |--------|---------|----------
 |  tag   | length  |  value  |

 ASN.1 allows for tags to be of "High-tag-number form" which allows a tag to
 be two or more octets, but that is not supported by this class. A tag is
 only 1 byte. Bits 1-5 give the tag number (ie the data type within a
 particular 'class'), 6 indicates whether or not the ASN.1 value is
 constructed from other ASN.1 values, and bits 7 and 8 give the 'class'. If
 bits 7 and 8 are both zero, the class is UNIVERSAL. If only bit 7 is set,
 then the class is APPLICATION. If only bit 8 is set, then the class is
 CONTEXT_SPECIFIC. If both bits 7 and 8 are set, then the class is PRIVATE.
 The tag numbers for the data types for the class UNIVERSAL are listed below:

 UNIVERSAL 0 Reserved for use by the encoding rules
 UNIVERSAL 1 Boolean type
 UNIVERSAL 2 Integer type
 UNIVERSAL 3 Bitstring type
 UNIVERSAL 4 Octetstring type
 UNIVERSAL 5 Null type
 UNIVERSAL 6 Object identifier type
 UNIVERSAL 7 Object descriptor type
 UNIVERSAL 8 External type and Instance-of type
 UNIVERSAL 9 Real type
 UNIVERSAL 10 Enumerated type
 UNIVERSAL 11 Embedded-pdv type
 UNIVERSAL 12 UTF8String type
 UNIVERSAL 13 Relative object identifier type
 UNIVERSAL 14-15 Reserved for future editions
 UNIVERSAL 16 Sequence and Sequence-of types
 UNIVERSAL 17 Set and Set-of types
 UNIVERSAL 18-22, 25-30 Character string types
 UNIVERSAL 23-24 Time types

 The length of an ASN.1 structure is specified after the tag identifier.
 There is a definite form and an indefinite form. The indefinite form may
 be used if the encoding is constructed and not all immediately available.
 The indefinite form is encoded using a length byte with only the 8th bit
 set. The end of the constructed object is marked using end-of-contents
 octets (two zero bytes).

 The definite form looks like this:

 The length may take up 1 or more bytes, it depends on the length of the
 value of the ASN.1 structure. DER encoding requires that if the ASN.1
 structure has a value that has a length greater than 127, more than 1 byte
 will be used to store its length, otherwise just one byte will be used.
 This is strict.

 In the case that the length of the ASN.1 value is less than 127, 1 octet
 (byte) is used to store the "short form" length. The 8th bit has a value of
 0 indicating the length is "short form" and not "long form" and bits 7-1
 give the length of the data. (The 8th bit is the left-most, most significant
 bit: also known as big endian or network format).

 In the case that the length of the ASN.1 value is greater than 127, 2 to
 127 octets (bytes) are used to store the "long form" length. The first
 byte's 8th bit is set to 1 to indicate the length is "long form." Bits 7-1
 give the number of additional octets. All following octets are in base 256
 with the most significant digit first (typical big-endian binary unsigned
 integer storage). So, for instance, if the length of a value was 257, the
 first byte would be set to:

 10000010 = 130 = 0x82.

 This indicates there are 2 octets (base 256) for the length. The second and
 third bytes (the octets just mentioned) would store the length in base 256:

 octet 2: 00000001 = 1 * 256^1 = 256
 octet 3: 00000001 = 1 * 256^0 = 1
 total = 257

 The algorithm for converting a js integer value of 257 to base-256 is:

 var value = 257;
 var bytes = [];
 bytes[0] = (value >>> 8) & 0xFF; // most significant byte first
 bytes[1] = value & 0xFF;        // least significant byte last

 On the ASN.1 UNIVERSAL Object Identifier (OID) type:

 An OID can be written like: "value1.value2.value3...valueN"

 The DER encoding rules:

 The first byte has the value 40 * value1 + value2.
 The following bytes, if any, encode the remaining values. Each value is
 encoded in base 128, most significant digit first (big endian), with as
 few digits as possible, and the most significant bit of each byte set
 to 1 except the last in each value's encoding. For example: Given the
 OID "1.2.840.113549", its DER encoding is (remember each byte except the
 last one in each encoding is OR'd with 0x80):

 byte 1: 40 * 1 + 2 = 42 = 0x2A.
 bytes 2-3: 128 * 6 + 72 = 840 = 6 72 = 6 72 = 0x0648 = 0x8648
 bytes 4-6: 16384 * 6 + 128 * 119 + 13 = 6 119 13 = 0x06770D = 0x86F70D

 The final value is: 0x2A864886F70D.
 The full OID (including ASN.1 tag and length of 6 bytes) is:
 0x06062A864886F70D
|#
;;; Code:
(in-package :dat/asn1)


(defun copy-to-lisp-vector (src vector count)
  (declare (octet-vector vector)
           (fixnum count)
           (optimize (safety 0) (speed 3)))
  (clone-octets-from-alien src vector count))

(defun asn1-string-octet-vector (asn1-string)
  (let* ((data (asn1-string-data asn1-string))
         (length (asn1-string-length asn1-string))
         (vector (io/static:make-static-vector length)))
    (copy-to-lisp-vector data vector length)
    vector))

(definline asn1-iastring-char-p (byte)
  (declare (type octet byte)
           (optimize (speed 3) (safety 0)))
  (< byte #x80))

(definline asn1-iastring-p (bytes)
  (declare (octet-vector bytes)
           (optimize (speed 3) (safety 0)))
  (every #'asn1-iastring-char-p bytes))

(defgeneric decode-asn1-string (self type))

(defmethod decode-asn1-string (self (type (eql #.(v-asn1 :ia5string))))
  (let ((bytes (asn1-string-octet-vector self)))
    (if (asn1-iastring-p self)
        (sb-ext:octets-to-string bytes :external-format :ascii)
        (error 'invalid-asn1-string :type #.(v-asn1 :ia5string)))))

(defun asn1-printable-char-p (byte)
  (declare (type (unsigned-byte 8) byte)
           (optimize (speed 3) (safety 0)))
  (cond
    ;; a-z
    ((and (>= byte #.(char-code #\a))
          (<= byte #.(char-code #\z)))
     t)
    ;; '-/
    ((and (>= byte #.(char-code #\'))
          (<= byte #.(char-code #\/)))
     t)
    ;; 0-9
    ((and (>= byte #.(char-code #\0))
          (<= byte #.(char-code #\9)))
     t)
    ;; A-Z
    ((and (>= byte #.(char-code #\A))
          (<= byte #.(char-code #\Z)))
     t)
    ;; other
    ((= byte #.(char-code #\ )) t)
    ((= byte #.(char-code #\:)) t)
    ((= byte #.(char-code #\=)) t)
    ((= byte #.(char-code #\?)) t)))

(definline asn1-printable-string-p (bytes)
  (declare (octet-vector bytes)
           (optimize (speed 3) (safety 0)))
  (every #'asn1-printable-char-p bytes))

(defmethod decode-asn1-string (self (type (eql #.(v-asn1 :printablestring))))
  (let* ((bytes (asn1-string-octet-vector self)))
    (if (asn1-printable-string-p bytes)
        (sb-ext:octets-to-string bytes :external-format :ascii)
        (error 'invalid-asn1-string :type #.(v-asn1 :printablestring)))))

(defmethod decode-asn1-string (self (type (eql #.(v-asn1 :utf8string))))
  (let* ((data (asn1-string-data self))
         (length (asn1-string-length self))
         (vec (make-octets length)))
    (clone-octets-from-alien data vec length)
    (sb-ext:octets-to-string vec :external-format :utf-8 :end length)))
    

(defmethod decode-asn1-string (self (type (eql #.(v-asn1 :universalstring))))
  (let ((len (asn1-string-length self))
        (data (asn1-string-data self)))
    (if (= 0 (mod len 4))
        (let ((vec (make-octets len)))
          (clone-octets-from-alien data vec len)
          (sb-ext:octets-to-string vec :external-format :utf32))
        (error 'invalid-asn1-string :type '+v-asn1-universalstring+))))

(definline asn1-teletex-char-p (byte)
  (declare (octet byte)
           (optimize (speed 3) (safety 0)))
  (and (>= byte #x20) (< byte #x80)))

(definline asn1-teletex-string-p (bytes)
  (declare (octet-vector bytes)
           (optimize (speed 3) (safety 0)))
  (every #'asn1-teletex-char-p bytes))

(defmethod decode-asn1-string (self (type (eql #.(v-asn1 :teletexstring))))
  (let ((bytes (asn1-string-octet-vector self)))
    (if (asn1-teletex-string-p bytes)
        (sb-ext:octets-to-string bytes :external-format :ascii)
        (error 'invalid-asn1-string :type #.(v-asn1 :teletexstring)))))

(defmethod decode-asn1-string (self (type (eql #.(v-asn1 :bmpstring))))
  (if (= 0 (mod (length self) 2))
      (let* ((data (asn1-string-data self))
             (len (asn1-string-length self))
             (vec (make-octets len)))
        (clone-octets-from-alien data vec len)
        (sb-ext:octets-to-string vec :external-format :utf-16/be))
      (error 'invalid-asn1-string :type (v-asn1 :bmpstring))))

(defun try-get-asn1-string-data (asn1-string allowed-types)
  (let ((type (asn1-string-type asn1-string)))
    (assert (member (v-asn1 (asn1-string-type asn1-string)) allowed-types) nil "Invalid asn1 string type")
    (decode-asn1-string asn1-string type)))

;; ASN1 Times are represented with ASN1 Strings
(defun decode-asn1-time (asn1-time)
  (when (zerop (asn1-time-check asn1-time))
    (error "asn1-time is not a syntactically valid ASN1 UTCTime"))
  (let ((time-string (sb-ext:octets-to-string (asn1-string-octet-vector asn1-time)
                                              :external-format :ascii)))
    (let* ((utctime-p (= 1 (asn1-utctime-check asn1-time)))
           (year-len (if utctime-p 2 4))
           (year-part (parse-integer (subseq time-string 0 year-len)))
           (year (if utctime-p
                     (if (>= year-part 50)
                         (+ 1900 year-part)
                         (+ 2000 year-part))
                     year-part)))
      (flet ((get-element-after-year (position)
               (parse-integer
                (subseq time-string
                        (+ position year-len)
                        (+ position year-len 2)))))
        (let ((month  (get-element-after-year 0))
              (day    (get-element-after-year 2))
              (hour   (get-element-after-year 4))
              (minute (get-element-after-year 6))
              (second (get-element-after-year 8)))
          (encode-universal-time second minute hour day month year 0))))))

(defmethod deserialize (from (format (eql :asn1)) &key v-asn1)
  (decode-asn1-string from (if v-asn1 (v-asn1 v-asn1) (asn1-string-type from))))
