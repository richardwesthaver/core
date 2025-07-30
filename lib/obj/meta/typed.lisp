;;; obj/meta/typed.lisp --- Typed meta-objects

;; - typed-slot-class

;; inspired by:
;; https://allegrograph.com/fixed-indices-speed-up-slot-access-in-allegro-cl/

;; may implement fixed.lisp separately.. we'll see.

;;; Commentary:

;; I still need to investigate what the actual behavior is in
;; SBCL.

;; - What sort of type checking is performed on slot-access, when that
;;   slot has type information? Does this vary at different compile levels?

;; - What is the performance impact of injecting additional
;;   slot-accessor type information? For example, declare as
;;   function-type with a typed result.

;;; Code:
(in-package :obj/meta/typed)

(declaim (type hash-table array-type-to-byte byte-to-array-type))
(defvar array-type-to-byte (make-hash-table :test 'equalp))
(defvar byte-to-array-type (make-hash-table :test 'equalp))
(sb-impl::robinhood-hashset-storage sb-kernel::*array-type-hashset*
*ctype-hashsets*
(setf (gethash 't array-type-to-byte) #x00)
(setf (gethash 'base-char array-type-to-byte) #x01)
(setf (gethash 'character array-type-to-byte) #x02)
(setf (gethash 'single-float array-type-to-byte) #x03)
(setf (gethash 'double-float array-type-to-byte) #x04)
(setf (gethash '(complex single-float) array-type-to-byte) #x05)
(setf (gethash '(complex double-float) array-type-to-byte) #x06)
(setf (gethash 'fixnum array-type-to-byte) #x07)
(setf (gethash 'bit array-type-to-byte) #x08)

(defun type-num (obj)
  "Define a type order; no guarantee that backend and front-end match
   so we can't iterate over types, just all members of a give type class
   (i.e. numbers, etc)"
  (cond ((numberp obj) 1)
        ((characterp obj) 1)
        ((symbolp obj) 13)
        ((stringp obj) 2)
        ((subtypep (type-of obj) 'stored) 15)
        ((consp obj) 16)
        ((subtypep (type-of obj) 'standard-object) 18)
        ((pathnamep obj) 12)
        ((hash-table-p obj) 17)
        ((subtypep (type-of obj) 'structure-object) 20)
        ((complexp obj) 22)))

(defun type<= (obj1 obj2)
  (<= (type-num obj1) (type-num obj2)))

(defun type< (obj1 obj2)
  (< (type-num obj1) (type-num obj2)))

(defun type= (obj1 obj2)
  (= (type-num obj1) (type-num obj2)))

(defun array-type= (t1 t2)
  (and (subtypep t1 t2) (subtypep t2 t1)))

(let ((counter 8))
  (loop for i from 2 to 65
        for spec = (list 'unsigned-byte i)
        for uspec = (upgraded-array-element-type spec)
        when (array-type= spec uspec)
        do
        (setf (gethash spec array-type-to-byte) (incf counter)))
  (loop for i from 2 to 65
        for spec = (list 'signed-byte i)
        for uspec = (upgraded-array-element-type spec)
        when (array-type= spec uspec)
        do
        (setf (gethash spec array-type-to-byte) (incf counter))))

(loop for key being the hash-key of array-type-to-byte 
      using (hash-value value)
      do
      (setf (gethash value byte-to-array-type) key))

(defun array-type-from-byte (b)
  (gethash b byte-to-array-type))

(defun byte-from-array-type (ty)
  (the (unsigned-byte 8) (gethash ty array-type-to-byte)))

(defun int-byte-spec (position)
  "Shared byte-spec peformance hack; not thread safe so removed
   from use for serializer2"
  (declare (type (unsigned-byte 24) position))
  (byte 32 (* 32 position)))
