;;; rand.lisp --- Random Utils

;; 

;;; Code:
(in-package :std/rand)

;;; random
(defvar *simple-charset* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  "The simple ascii [a-zA-Z0-9] charset.")

(defun random-elt (seq)
  "Return a random element from SEQ."
  (elt seq (random (length seq))))

(defun random-ref (vec)
  "Return a random element from VEC."
  (aref vec (random (length vec))))

(defun random-char ()
  "Return a random character."
  (random-ref *simple-charset*))

(defun random-chars (dim)
  "Return an array of random characters with dimensions DIM."
  (let ((r (make-array dim :element-type 'character)))
    (dotimes (i (array-total-size r) r)
      (setf (row-major-aref r i) (random-char)))))

(defun random-byte () 
  "Return a random byte."
  (random 255))

(defun random-bytes (dim)
  "Return an array of random bytes with dimensions DIM."
  (let ((r (make-array dim :element-type 'octet)))
    (dotimes (i (array-total-size r) r)
      (setf (row-major-aref r i) (random-byte)))))

(defun random-booleans (num &optional (probability 2))
  (loop for i from 1 to num
        collect (zerop (random probability))))

(defmacro random-do (&rest statements)
  (let ((length (length statements)))
    `(case (random ,length)
       ,@(loop for x from 0 to length
               for statement in statements
               collect `(,x ,statement)))))

;; from SBCL tests
(defun shuffle (sequence)
  "Shuffle the elements of SEQUENCE."
  (typecase sequence
    (list
     (coerce (shuffle (coerce sequence 'vector)) 'list))
    (vector ; destructive
     (let ((vector sequence))
       (loop for lim from (1- (length vector)) downto 0
             for chosen = (random (1+ lim))
             unless (= chosen lim)
             do (rotatef (aref vector chosen) (aref vector lim)))
       vector))))
