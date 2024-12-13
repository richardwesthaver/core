;;; lib/obj/hash/hasher.lisp --- Hash Functions

;;

;;; Code:
(in-package :obj/hash)

(eval-always
  (defvar *global-hasher* #'sxhash))

;; TODO 2024-05-24: do better
(sb-ext:define-load-time-global *global-hash* (funcall *global-hasher* (get-universal-time)))

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

(defgeneric hash-object (obj)
  (:method ((obj t))
    (hash-object-address obj)))

(defun hash-object-address (obj &optional (test *global-hasher*))
  "Given some object OBJ, lookup the address with
  SB-KERNEL:GET-LISP-OBJ-ADDRESS and return a hash."
  (funcall test (sb-kernel:get-lisp-obj-address obj)))

(defun object-address-hash-equalp (a b)
  (= (hash-object-address a) (hash-object-address b)))

(sb-ext:define-hash-table-test object-address-hash-equalp hash-object-address)

;; from quicklisp src
(defun dumb-string-hash (str)
  "Produce a six-character hash of STRING."
  (let ((hash #xD13CCD13))
    (loop for char across str
          for value = (char-code char)
          do
          (setf hash (logand #xFFFFFFFF
                             (logxor (ash hash 5)
                                     (ash hash -27)
                                     value))))
    (subseq (format nil "~(~36,6,'0R~)" (mod hash 88888901))
            0 6)))

;; sb-lockless::multiplicative-hash

;;; Perfect Hashes

#|
(setq *h* (sb-c:make-perfect-hash-lambda
           (map '(array (unsigned-byte 32) 1) (lambda (x) (ldb (byte 32 0) (sxhash x)))
                '(a b c d e f g h i j k l m n o p))))
|#
