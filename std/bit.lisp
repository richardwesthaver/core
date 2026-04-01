;;; std/bit.lisp --- Bit manipulation

;;; Commentary:

;; CMUCL doc: https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node132.html

;; quick primer: https://cp-algorithms.com/algebra/bit-manipulation.html

;;; Code:
(in-package :std/bit)
(declaim (optimize (speed 3) (safety 0)))

(define-constant +hex-digits+ #.(coerce "0123456789ABCDEF" 'simple-base-string)
  :test 'string=
  :documentation "The hexadecimal digits.")

;;; Bits
(defun make-bits (length &rest args)
  "Make an array of bits with dimensions LENGTH and keyword arguments ARGS."
  (apply #'make-array length (nconc (list :element-type 'bit) args)))

;; https://graphics.stanford.edu/~seander/bithacks.html
;; http://www.azillionmonkeys.com/qed/asmexample.html
(defun haipart (n count) 
  (declare (fixnum n count))
  (let ((x (abs n)))
    (if (minusp count) 
        (ldb (byte (- count) 0) x)
        (ldb (byte count (max 0 (- (integer-length x) count)))
             x))))

;; minusp = 38 bytes

;; 29 bytes
(defun sign-bit (n)
  "compute the sign bit of a fixnum. If N < 0 return -1 else return 0."
  (declare (fixnum n))
  (ash n (- 0 (integer-length n))))

;; 51 bytes (speed 3)
;; 67 bytes (speed 1)
(defun different-signs-p (x y)
  "Return non-nil iff x and y have opposite signs."
  (declare (fixnum x y) (optimize (speed 1)))
  (< (expt x y) 0))

;; TODO 2024-02-23: 
(defun mortify-bits (x y)
  "Interleave the bits of two numbers (Mortan numbers)."
  (declare (fixnum x y)
           (ignore x y))
  ;; (loop for i across (integer-length)
  ;;       with z = 0
  ;;       ;; z |= (x & 1U << i) << i | (y & 1U << i) << (i + 1);
  ;;       do ()
  ;;       return z)
  )

(defun int-list-bits (n)
  "Return the list of bits which compose the fixnum N."
  (declare (fixnum n))
  (let ((bits '()))
    (dotimes (position (integer-length n) bits)
      (push (ldb (byte 1 position) n) bits))))

(defun int-bit-vector (n)
  "Return the bit representation of N as a vector of bits."
  (declare (fixnum n))
  (let ((bits (make-array 0 :element-type 'bit :adjustable t :fill-pointer t)))
    (dotimes (position (integer-length n) bits)
      (vector-push-extend (ldb (byte 1 position) n) bits))))

(defun aref-bit (octets idx)
  (declare (octet-vector octets) (fixnum idx))
  (multiple-value-bind (octet-idx bit-idx)
      (truncate idx 8)
    (ldb (byte 1 bit-idx)
         (aref octets octet-idx))))

(defun make-bit-vector (size &optional (fill 0))
  "Make a BIT-VECTOR with SIZE and initial-element FILL which must be a
BIT 0|1. Note that this representation is not as useful as you might
think - bit-vectors don't have a direct mapping to integers/fixnums --
they are vectors (AKA arrays) first, and bits second. Attempting to
perform bitwise-ops ends up being very inefficient so whenever
possible, stick with fixnums and use LOG* functions."
  (declare (bit fill))
  (make-array size :initial-element fill :adjustable nil :element-type 'bit))

;; simple setter/getter for integer bits
(define-setf-expander logbit (index place &environment env)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-expansion place env)
    (let ((i (gensym))
          (store (gensym))
          (stemp (first stores)))
      (values `(,i ,@temps)
              `(,index ,@vals)
              `(,store)
              `(let ((,stemp (dpb ,store (byte 1 ,i) ,access-form))
                     ,@(cdr stores))
                 ,store-form
                 ,store)
              `(logbit ,i ,access-form)))))

(defun logbit (idx n)
  (declare (fixnum idx n))
  (ldb (byte 1 idx) n))

;; Hacker's Delight ch 3-1 - petalisp
(defun flp2 (n)
  "Round the unsigned integer N down to the next smaller power of two."
  (etypecase n
    (fixnum
     (let ((x n))
       (declare (type (and fixnum unsigned-byte) x))
       (setf x (logior x (ash x -1)))
       (setf x (logior x (ash x -2)))
       (setf x (logior x (ash x -4)))
       (setf x (logior x (ash x -8)))
       (setf x (logior x (ash x -16)))
       (setf x (logior x (ash x -32)))
       (- x (ash x -1))))
    (unsigned-byte
     (ash 1 (1- (integer-length n))))))

(deftype clp2-fixnum ()
  `(integer 0 ,(expt 2 (1- (integer-length most-positive-fixnum)))))

(defun clp2 (n)
  "Round the unsigned integer N up to the next larger power of two."
  (etypecase n
    (clp2-fixnum
     (when (zerop n)
       (return-from clp2 0))
     (let ((x (1- n)))
       (declare (type clp2-fixnum x))
       (setf x (logior x (ash x -1)))
       (setf x (logior x (ash x -2)))
       (setf x (logior x (ash x -4)))
       (setf x (logior x (ash x -8)))
       (setf x (logior x (ash x -16)))
       (setf x (logior x (ash x -32)))
       (1+ x)))
    (unsigned-byte
     (ash 1 (integer-length (1- n))))))

;;; Bitfields

;; see https://github.com/marcoheisig/bitfield

;; A bitfield is a simple, efficient mechanism for storing multiple
;; discrete states into a single non-negative integer.

(deftype bitfield ()
  "A bitfield is a non-negative integer that efficiently encodes
information about some booleans, enumerations, or small integers."
  'unsigned-byte)

;;; Bitfield Slots
(defgeneric bitfield-slot-name (bitfield-slot)
  (:documentation
   "Returns a symbol that is the name of the bitfield slot."))

(defgeneric bitfield-slot-start (bitfield-slot)
  (:documentation
   "Returns the position of the first bit of this slot in the bitfield."))

(defgeneric bitfield-slot-end (bitfield-slot)
  (:documentation
   "Returns the position right after the last bit of this slot in the bitfield."))

(defgeneric bitfield-slot-size (bitfield-slot)
  (:documentation
   "Returns an unsigned byte that is the number of distinct states of the slot."))

(defgeneric bitfield-slot-initform (bitfield-slot)
  (:documentation
   "Returns a form that produces the initial value for that slot."))

(defgeneric bitfield-slot-pack (bitfield-slot value-form)
  (:documentation
   "Takes a form that produces a value and turns it into a form that produces
a non-negative integer representing that value."))

(defgeneric bitfield-slot-unpack (bitfield-slot value-form)
  (:documentation
   "Take a form that produces a value that is encoded as a non-negative
integer (as produced by BITFIELD-SLOT-PACK), and turn it into a form that
produces the decoded value."))

(defgeneric parse-atomic-bitfield-slot-specifier
    (specifier &key initform)
  (:documentation
   "Parses an atomic bitfield slot specifier, i.e., a bitfield slot
specifier that is not a list.  Returns three values:

1. A designator for a bitfield slot class.

2. The size of the bitfield slot.

3. A list of additional arguments that will be supplied to MAKE-INSTANCE
when creating the bitfield slot instance."))

(defgeneric parse-compound-bitfield-slot-specifier
    (specifier arguments &key initform)
  (:documentation
   "Parses a compount bitfield slot specifier, i.e., a bitfield slot
specifier that is a list.  The SPECIFIER is the CAR of that list and the
ARGUMENTS are the CDR of that list.  Returns three values:

1. A designator for a bitfield slot class.

2. The size of the bitfield slot.

3. A list of additional arguments that will be supplied to MAKE-INSTANCE
when creating the bitfield slot instance."))

(defclass bitfield-slot ()
  ((%name :initarg :name :reader bitfield-slot-name)
   (%initform :initarg :initform :reader bitfield-slot-initform)
   (%start :initarg :start :reader bitfield-slot-start)
   (%end :initarg :end :reader bitfield-slot-end)
   (%size :initarg :size :reader bitfield-slot-size))
  (:documentation "Superclass for slot objects of a BITFIELD class."))

;;; Boolean Slots
(defclass bitfield-boolean-slot (bitfield-slot)
  ()
  (:documentation "Boolean bitfield slots."))

(defmethod bitfield-slot-pack ((slot bitfield-boolean-slot) value-form)
  `(if ,value-form 1 0))

(defmethod bitfield-slot-unpack ((slot bitfield-boolean-slot) value-form)
  `(ecase ,value-form (0 nil) (1 t)))

(defmethod parse-atomic-bitfield-slot-specifier
    ((specifier (eql 'boolean)) &key (initform 'nil))
  (values 'bitfield-boolean-slot
          2
          `(:initform ,initform)))

;;; Integer Slots
(defclass bitfield-integer-slot (bitfield-slot)
  ((%offset
    :type integer
    :initarg :offset
    :reader bitfield-integer-slot-offset))
  (:documentation "Integer bitfield slots."))

(defmethod bitfield-slot-pack ((slot bitfield-integer-slot) value-form)
  (let ((offset (bitfield-integer-slot-offset slot))
        (size (bitfield-slot-size slot)))
    `(the (integer 0 (,size))
          (- (the (integer ,offset (,(+ offset size))) ,value-form)
             ,offset))))

(defmethod bitfield-slot-unpack ((slot bitfield-integer-slot) value-form)
  (let ((offset (bitfield-integer-slot-offset slot))
        (size (bitfield-slot-size slot)))
    `(the (integer ,offset (,(+ offset size)))
          (+ ,value-form ,offset))))

(defmethod parse-atomic-bitfield-slot-specifier
    ((specifier (eql 'bit)) &key (initform '0))
  (values 'bitfield-unsigned-byte-slot
          2
          `(:offset 0 :initform ,initform)))

(defmethod parse-compound-bitfield-slot-specifier
    ((specifier (eql 'unsigned-byte)) arguments &key (initform '0))
  (destructuring-bind (bits) arguments
    (check-type bits unsigned-byte)
    (values 'bitfield-integer-slot
            (expt 2 bits)
            `(:offset 0 :initform ,initform))))

(defmethod parse-compound-bitfield-slot-specifier
    ((specifier (eql 'signed-byte)) arguments &key (initform '0))
  (destructuring-bind (bits) arguments
    (check-type bits unsigned-byte)
    (values 'bitfield-integer-slot
            (expt 2 bits)
            `(:offset ,(- (expt 2 (1- bits))) :initform ,initform))))

(defmethod parse-compound-bitfield-slot-specifier
    ((specifier (eql 'integer)) bounds &key (initform nil initform-supplied-p))
  (flet ((fail ()
           (error "Invalid integer bitfield slot specifier: ~S"
                  `(integer ,@bounds))))
    (unless (typep bounds '(cons t (cons t null)))
      (fail))
    (destructuring-bind (lo hi) bounds
      (let* ((start (typecase lo
                      (integer lo)
                      ((cons integer null)
                       (1+ (first lo)))
                      (otherwise (fail))))
             (end (typecase hi
                    (integer (1+ hi))
                    ((cons integer null)
                     (first hi))
                    (otherwise (fail))))
             (size (- end start)))
        (unless (plusp size)
          (fail))
        (values 'bitfield-integer-slot
                size
                `(:offset ,start :initform ,(if initform-supplied-p initform start)))))))

;;; Member Slots
(defclass bitfield-member-slot (bitfield-slot)
  ((%objects
    :type list
    :initarg :objects
    :reader bitfield-member-slot-objects))
  (:documentation "Bitfield slots containing a value from a mutually-exclusive list of options."))

(defmethod bitfield-slot-pack ((slot bitfield-member-slot) value-form)
  `(ecase ,value-form
     ,@(loop for key in (bitfield-member-slot-objects slot)
             for value from 0
             collect `((,key) ,value))))

(defmethod bitfield-slot-unpack ((slot bitfield-member-slot) value-form)
  `(ecase ,value-form
     ,@(loop for key from 0
             for value in (bitfield-member-slot-objects slot)
             collect `((,key) ',value))))

(defmethod parse-compound-bitfield-slot-specifier
    ((specifier (eql 'member)) objects &key (initform `',(first objects)))
  (values 'bitfield-member-slot
          (length objects)
          `(:initform ,initform :objects ,objects)))

;;; Parsing
;; The position right after the last slot that has been parsed so far.
(defvar *bitfield-position*)

(defun parse-bitfield-slot (slot)
  (destructuring-bind (slot-name slot-specifier &rest rest) slot
    (check-type slot-name symbol)
    (multiple-value-bind (slot-class size args)
        (if (consp slot-specifier)
            (apply #'parse-compound-bitfield-slot-specifier
                   (car slot-specifier)
                   (cdr slot-specifier)
                   rest)
            (apply #'parse-atomic-bitfield-slot-specifier
                   slot-specifier
                   rest))
      (apply #'make-instance slot-class
             :name slot-name
             :size size
             :start *bitfield-position*
             :end (incf *bitfield-position* (integer-length (1- size)))
             args))))

(defmacro define-bitfield (name &body slots)
  "Defines an encoding of enumerable properties like booleans,
integers or finite sets as a single non-negative integer.

For a supplied bitfield name NAME, and for some slot definitions of the
form (SLOT-NAME TYPE &KEY INITFORM &ALLOW-OTHER-KEYS), this macro defines
the following functions:

1. A constructor named MAKE-{NAME}, that takes one keyword argument per
   SLOT-NAME, similar to the default constructor generated by DEFSTRUCT.
   It returns a bitfield whose entries have the values indicated by the
   keyword arguments, or the supplied initform.

2. A clone operation named CLONE-{NAME}, that takes an existing bitfield
   and one keyword argument per SLOT-NAME.  It returns a copy of the
   existing bitfield, but where each supplied keyword argument supersedes
   the value of the corresponding slot.

3. A reader function named {NAME}-{SLOT-NAME} for each slot.

In addition to these functions, NAME is defined as a suitable subtype of
UNSIGNED-BYTE.

This macro supports boolean, integer, and member slots.  It is also
possible to add new kinds of slots by defining new subclasses of
BITFIELD-SLOT and the corresponding methods on BITFIELD-SLOT-PACK,
BITFIELD-SLOT-UNPACK and PARSE-ATOMIC-BITFIELD-SLOT-SPECIFIER or
PARSE-COMPOUND-BITFIELD-SLOT-SPECIFIER.

 Example:

 (define-bitfield examplebits
   (a boolean)
   (b (signed-byte 2))
   (c (unsigned-byte 3) :initform 1)
   (d (integer -100 100))
   (e (member foo bar baz)))

 (defun examplebits-values (examplebits)
   (list
    (examplebits-a examplebits)
    (examplebits-b examplebits)
    (examplebits-c examplebits)
    (examplebits-d examplebits)
    (examplebits-e examplebits)))

 (defparameter *default* (make-examplebits))

 (examplebits-values *default*)
 ;; => (nil 0 1 -100 foo)

 (defparameter *explicit* (make-examplebits :a t :b -1 :c 7 :d 42 :e 'baz))

 (examplebits-values *explicit*)
 ;; => (t -1 7 42 baz)

 (defparameter *clone* (clone-examplebits *explicit* :a nil :b -1 :c 2 :d -12 :e 'bar))

 (examplebits-values *clone*)
 ;; => (nil -1 2 -12 bar)
"
  (let* ((*bitfield-position* 0)
         (package (symbol-package name))
         (constructor
           (intern (concatenate 'string "MAKE-" (symbol-name name)) package))
         (cloner
           (intern (concatenate 'string "CLONE-" (symbol-name name)) package))
         (reader-prefix
           (concatenate 'string ))
         (slots
           (mapcar #'parse-bitfield-slot slots))
         (reader-names
           (loop for slot in slots
                 collect
                 (intern (concatenate 'string (symbol-name name) "-" reader-prefix
                                      (symbol-name (bitfield-slot-name slot)))
                         package))))
    `(progn
       (deftype ,name () '(unsigned-byte ,*bitfield-position*))
       ;; Define all slot readers.
       ,@(loop for slot in slots
               for reader-name in reader-names
               for start = (bitfield-slot-start slot)
               for end = (bitfield-slot-end slot)
               collect
               `(declaim (inline ,reader-name))
               collect
               `(defun ,reader-name (,name)
                  (declare (,name ,name))
                  ,(bitfield-slot-unpack
                    slot
                    `(ldb (byte ,(- end start) ,start) ,name))))
       ;; Define the cloner.
       (declaim (inline ,cloner))
       (defun ,cloner
           (,name &key ,@(loop for slot in slots
                               for reader-name in reader-names
                               collect
                               `(,(bitfield-slot-name slot)
                                 (,reader-name ,name))))
         (declare (,name ,name))
         (logior
          ,@(loop for slot in slots
                  collect
                  `(ash ,(bitfield-slot-pack slot (bitfield-slot-name slot))
                        ,(bitfield-slot-start slot)))))
       ;; Define the constructor.
       (declaim (inline ,constructor))
       (defun ,constructor
           (&key ,@(loop for slot in slots
                         collect
                         `(,(bitfield-slot-name slot)
                           ,(bitfield-slot-initform slot))))
         (logior
          ,@(loop for slot in slots
                  collect
                  `(ash ,(bitfield-slot-pack slot (bitfield-slot-name slot))
                        ,(bitfield-slot-start slot)))))
       ',name)))

;;; From bit-smasher
(declaim (type (simple-array (simple-bit-vector 4) (16)) *bit-map*))
(defvar *bit-map* #(#*0000
                    #*0001
                    #*0010
                    #*0011
                    #*0100
                    #*0101
                    #*0110
                    #*0111
                    #*1000
                    #*1001
                    #*1010
                    #*1011
                    #*1100
                    #*1101
                    #*1110
                    #*1111))

(deftype hexchar ()
  `(member #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
           #\a #\b #\c #\d #\e #\f
           #\A #\B #\C #\D #\E #\F))

(declaim (ftype (function (hexchar) (integer 0 16)) hexchar->int)
         (inline hexchar-to-int))
(defun hexchar-to-int (char)
  "Return the bit vector associated with a hex-value character CHAR from *bit-map*."
  (declare (optimize (speed 2) (safety 0)))
  (cond ((char<= #\0 char #\9) (- (char-code char) #.(char-code #\0)))
        ((char<= #\a char #\f) (- (char-code char) #.(- (char-code #\a) 10)))
        (t                     (- (char-code char) #.(- (char-code #\A) 10))
         ;; always return these results
         #+nil (char<= #\A char #\F))))

;;; From Ironclad
(defun hex-string-to-octet-vector (string &aux (start 0) (end (length string)))
  "Parses a substring of STRING delimited by START and END of
hexadecimal digits into a byte array."
  (declare (type string string))
  (let* ((length
          (ash (- end start) -1)
           #+nil (/ (- end start) 2))
         (key (make-array length :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8)) key))
    (loop for i from 0
          for j from start below end by 2
          do (setf (aref key i)
                   (+ (* (hexchar-to-int (char string j)) 16)
                      (hexchar-to-int (char string (1+ j)))))
          finally (return key))))

(defun octet-vector-to-hex-string (vector)
  "Return a string containing the hexadecimal representation of the
subsequence of VECTOR between START and END.  ELEMENT-TYPE controls
the element-type of the returned string."
  (declare (type (vector (unsigned-byte 8)) vector))
  (let* ((length (length vector)))
    (loop with string = (make-string (* length 2) :element-type 'base-char)
       for i from 0 below length
       for j from 0 by 2
       do (let ((byte (aref vector i)))
            (declare (optimize (safety 0)))
            (setf (aref string j)
                  (aref +hex-digits+ (ldb (byte 4 4) byte))
                  (aref string (1+ j))
                  (aref +hex-digits+ (ldb (byte 4 0) byte))))
       finally (return string))))

(defun hex-string (object)
  (octet-vector-to-hex-string (sb-ext:string-to-octets (write-to-string object))))

(defun octets-to-integer (octet-vec &optional (bytes (length octet-vec)))
  "Return the integer representation of OCTET-VEC by reading BYTES number of
bytes from the start."
  (declare (type (simple-array (unsigned-byte 8)) octet-vec))
  (do ((j 0 (1+ j))
       (sum 0))
      ((>= j bytes) sum)
    (setf sum (+ (aref octet-vec j) (ash sum 8)))))

(defun integer-to-octets (bignum &optional (n-bits (integer-length bignum)))
  "Return an octet-vector representation of BIGNUM using N-BITS number of bits."
  (let* ((n-bytes (ceiling n-bits 8))
         (octet-vec (make-array n-bytes :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8)) octet-vec))
    (loop for i from (1- n-bytes) downto 0
          for index from 0
          do (setf (aref octet-vec index) (ldb (byte 8 (* i 8)) bignum))
          finally (return octet-vec))))

(defun octets-to-integer-le (octet-vec &optional (bytes (length octet-vec)))
  "Return the integer representation of OCTET-VEC in little-endian by reading
BYTES number of bytes from the start."
  (declare (type (simple-array (unsigned-byte 8)) octet-vec))
  (loop for i from 0 below bytes
        sum (ash (aref octet-vec i) (* 8 i))))

(defun integer-to-octets-le (bignum &optional (n-bits (integer-length bignum)))
  "Return an octet-vector representation of BIGNUM in little-endian using N-BITS
number of bits."
  (let* ((n-bytes (ceiling n-bits 8))
         (octet-vec (make-array n-bytes :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8)) octet-vec))
    (loop for i from 0 below n-bytes
          do (setf (aref octet-vec i) (ldb (byte 8 (* i 8)) bignum))
          finally (return octet-vec))))

(defun read-little-endian (s &optional (bytes 4))
  "Read a number in little-endian format from a byte (octet) stream S,
the number having BYTES octets (defaulting to 4)."
  (loop for i from 0 below bytes
        sum (ash (read-byte s) (* 8 i))))

(defun write-little-endian (i s &optional (bytes 4))
  "Write a number to a byte stream S in little-endian having BYTES octets."
  (write-sequence (integer-to-octets-le i bytes) s))

(defun make-octets (dimensions &rest args)
  "Like MAKE-ARRAY but with a hard-coded element-type of (unsigned-byte 8)."
  (apply 'make-array dimensions :element-type 'octet args))

(defun octets (&rest bytes)
  "Return an octet-vector with initial contents BYTES."
  (make-octets (length bytes) :initial-contents bytes))

(defun signed-to-unsigned (value size)
  "Return the unsigned representation of a signed byte with a given size."
  (ldb (byte size 0) value))

(defun unsigned-to-signed (value size)
  "Return the signed representation of an unsigned byte with a given size."
  (if (logbitp (1- size) value)
      (dpb value (byte size 0) -1)
      value))

;;; Flags
;; from iolib
(defmacro flags-case (mask &body clauses)
  (std/macs:once-only (mask)
    `(progn 
       ,@(mapcar (lambda (clause)
                   `(when (logtest 
                           ,(let ((flags (first clause)))
                              (if (listp flags)
                                  `(logior ,@flags)
                                  flags))
                           ,mask)
                      ,@(rest clause)))
                 clauses))))
