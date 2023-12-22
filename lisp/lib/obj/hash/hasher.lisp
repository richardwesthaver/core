;;; lib/obj/hash/hasher.lisp --- Hash Functions

;;

;;; Code:
(in-package :obj/hash)
(eval-always
  (defvar *global-hasher* #'sxhash))

(defconstant +global-hash+ 
  (if (boundp '+global-hash+)
      +global-hash+
      (funcall *global-hasher* (get-universal-time))))

(macrolet ((specialize (str body)       ; TODO 2023-12-21: test if this actually compiles to fastpath
             `(if (typep ,str '(simple-array character 1))
                  ,body
                  ,body)))
  (defun djb (string)
    (declare (string string)
             (optimize speed))
    (let ((hash 5381))
      (declare ((and unsigned-byte fixnum) hash))
      (specialize
       string
       (dotimes (n (min 6 (length string)))
         (setf hash
               (logand most-positive-fixnum
                       (logxor (* hash 33)
                               (char-code (schar string n)))))))
      hash)))

(defun hash-object-address (obj &optional (test *global-hasher*))
  "Given some object OBJ, lookup the address with
  SB-KERNEL:GET-LISP-OBJ-ADDRESS and return a hash."
  (funcall test (sb-kernel:get-lisp-obj-address obj)))

(defun object-address-hash-equalp (a b)
  (= (hash-object-address a) (hash-object-address b)))

(sb-ext:define-hash-table-test object-address-hash-equalp hash-object-address)

(defgeneric hash-object (obj))
