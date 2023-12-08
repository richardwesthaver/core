;;; std/alien.lisp --- foreign alien friends

;;; Commentary:

;; FFI in Lisp is somewhat different than FFI in other host langs. As
;; such, we usually refer to our Lispy FFI interfaces inline with the
;; CMUCL terminology: alien interfaces.

;; ref: https://www.sbcl.org/manual/#Foreign-Function-Interface for details

;; sb-alien is a high-level interface which automatically converts C
;; memory pointers to lisp objects and back, but this can be slow for
;; large or complex objects.

;; The lower-level interface is based on System Area Pointers (or
;; SAPs), which provide untyped access to foreign memory.

;; Objects which can't be automatically converted into Lisp values are
;; represented by objects of type ALIEN-VALUE.

;;; Code:
(uiop:define-package :std/alien
  (:nicknames :alien)
  (:use :cl :sb-vm :sb-ext :sb-c :std/base)
  (:use-reexport :sb-alien)
  (:export
   :setfa
   :copy-c-string
   :clone-strings
   :clone-octets-to-alien
   :clone-octets-from-alien
   :foreign-int-to-integer :foreign-int-to-bool :bool-to-foreign-int
   :defbytes
   :u1 :u2 :u3 :u4 :u8 :u16 :u24 :u32 :u64 :u128
   :i2 :i3 :i4 :i8 :i16 :i24 :i32 :i64 :i128
   :f16 :f24 :f32 :f64 :f128))

(in-package :std/alien)

;; (reexport-from :sb-vm
;; 	       :include
;; 	       '(:with-pinned-objects :with-pinned-object-iterator :with-code-pages-pinned
;; 		 :sanctify-for-execution))

(defun setfa (place from) 
  (loop for x across from
	for i from 0 below (length from)
	do (setf (deref place i) x)))

(defun copy-c-string (src dest &aux (index 0))
  (loop (let ((b (sb-sys:sap-ref-8 src index)))
          (when (= b 0)
            (setf (fill-pointer dest) index)
            (return))
          (setf (char dest index) (code-char b))
          (incf index))))

(defun clone-strings (list)
  (with-alien ((x (* (* char))
                  (make-alien (* char) (length list))))
    (unwind-protect
         (labels ((populate (list index function)
                    (if list
                        (let ((array (sb-ext:string-to-octets (car list) :null-terminate t)))
                          (sb-sys:with-pinned-objects (array)
                            (setf (deref x index) (sap-alien (sb-sys:vector-sap array) (* char)))
                            (populate (cdr list) (1+ index) function)))
                        (funcall function))))
           (populate list 0
                     (lambda ()
                       (loop for i below (length list)
                             do (print (cast (deref x i) c-string))))))
      (free-alien x))))

(defmacro clone-octets-to-alien (lispa aliena)
  (with-gensyms (i)
    `(loop for ,i from 0 below (length ,lispa)
        do (setf (deref ,aliena ,i)
                 (aref ,lispa ,i)))))

(defmacro clone-octets-from-alien (aliena lispa len)
  (with-gensyms (i)
    `(loop for ,i from 0 below ,len
           do (setf (aref ,lispa ,i)
                 (deref ,aliena ,i)))))

(defun foreign-int-to-integer (buffer size)
  "Check SIZE of int BUFFER. return BUFFER."
  (assert (= size (sb-alien:alien-size sb-alien:int :bytes)))
  buffer)

(defun foreign-int-to-bool (x size)
  (if (zerop (foreign-int-to-integer x size))
      nil
      t))

(defun bool-to-foreign-int (val)
  (if val 1 0))

;;; Bytes
(defmacro defbytes (&body bitsets)
  "For each cons-cell in BITSETS, define a new CAR-byte type for each
member of CDR."
  `(loop for set in ',bitsets
	 collect
	 (let* ((ty (car set))
		(pfx
		  (cond
		    ((eq 'signed-byte ty) "I")
		    ((eq 'unsigned-byte ty) "U")
		    ((eq 'float ty) "F")
		    (t (subseq (symbol-name ty) 0 1))))
		(nums (cdr set))
		r) ;result
	   (setf r
		 (mapc
		  (lambda (x)
		    `(deftype ,(symbolicate pfx (format 'nil "~a" x)) ()
		       (cons ,ty ,x)))
		       nums))
	   (cons ty r))))

(defbytes
  (unsigned-byte 1 2 3 4 8 16 24 32 64 128)
  (signed-byte 2 3 4 8 16 24 32 64 128)
  (float 16 24 32 64 128))
