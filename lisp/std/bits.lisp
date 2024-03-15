;;; bits.lisp --- Bit manipulation

;;; Commentary:

;; CMUCL doc: https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node132.html

;; quick primer: https://cp-algorithms.com/algebra/bit-manipulation.html

;;; Code:
(in-package :std)

;;; TYPES
;; Bytes aren't necessarily 8 bits wide in Lisp. OCTET is always 8
;; bits.
(deftype octet () '(unsigned-byte 8))
(deftype octet-vector (&optional length)
  `(simple-array octet (,length)))

;;; BITS
(defun make-bits (length &rest args)
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
  (declare (fixnum n))
  (let ((bits '()))
    (dotimes (position (integer-length n) bits)
      (push (ldb (byte 1 position) n) bits))))

(defun int-bit-vector (n)
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
