;;; alien.lisp --- foreign alien friends

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
(in-package :std)
;; (reexport-from :sb-vm
;;  	       :include
;;  	       '(:with-pinned-objects :with-pinned-object-iterator :with-code-pages-pinned
;;  		 :sanctify-for-execution))

(defun shared-object-name (name)
  "Return a filename with the correct extension for a shared library
on Linux and Darwin."
  (format nil "lib~a.~a" name #+darwin "dylib" #-darwin "so"))

(defmacro define-alien-loader (name &optional export)
  "Define a default loader function named load-NAME which calls
SB-ALIEN:LOAD-SHARED-OBJECT."
  (let ((fname (sb-int:symbolicate (format nil "~@:(load-~a~)" name))))
    `(prog1
       (defun ,fname (&optional save)
         (prog1 (sb-alien:load-shared-object (shared-object-name ',name) :dont-save (not save))
           (pushnew ,(sb-int:keywordicate name) *features*)))
       ,@(when export (list `(export '(,fname)))))))
       
(defmacro define-opaque (ty &optional no-export)
  `(prog1
       (define-alien-type ,ty (struct ,(symbolicate ty '-t)))
     ,(unless no-export `(export '(,ty)))))

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

;;; Bits
(defun make-bits (length &rest args)
  (apply #'make-array length (nconc '(:element-type bit) args)))

;;; Bytes
;; (defmacro defbytes (&body bitsets)
;;   "For each cons-cell in BITSETS, define a new CAR-byte type for each
;; member of CDR."
;;   `(loop for set in ',bitsets
;; 	 collect
;; 	 (let* ((ty (car set))
;; 		(pfx
;; 		  (cond
;; 		    ((eq 'signed-byte ty) "I")
;; 		    ((eq 'unsigned-byte ty) "U")
;; 		    ((eq 'float ty) "F")
;; 		    (t (subseq (symbol-name ty) 0 1))))
;; 		(nums (cdr set))
;; 		r) ;result
;; 	   (setf r
;; 		 (mapc
;; 		  (lambda (x)
;; 		    `(deftype ,(symbolicate pfx (format 'nil "~a" x)) ()
;; 		       (cons ,ty ,x)))
;; 		       nums))
;; 	   (cons ty r))))

;; (defbytes
;;   (unsigned-byte 1 2 3 4 8 16 24 32 64 128)
;;   (signed-byte 2 3 4 8 16 24 32 64 128)
;;   (float 16 24 32 64 128))
