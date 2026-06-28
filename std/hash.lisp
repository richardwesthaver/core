;;; std/hash.lisp --- Standard Hash Utilities

;;

;;; Commentary:

;; Hash-tables in Lisp are the O(1) older brother of alists and
;; plists. Sometimes it is convenient to use the cons-based siblings, but for
;; most instance of key -> value pairs you're often better off with a
;; hash-table where performance is critical.

;;; Code:
(in-package :std/hash)

(defun copy-hash (hash &optional test comb)
  "Return a copy of HASH.
Optional argument TEST specifies a new equality test to use for the
copy. Second optional argument COMB specifies a function to use to
combine the values of elements of HASH which collide in the copy due
to a new equality test specified with TEST."
  (let ((comb (when comb (fdefinition comb)))
        (copy (make-hash-table :test (or test (hash-table-test hash)))))
    (maphash (lambda (k v) (setf (gethash k copy)
                            (if (and (gethash k copy) comb)
                                (funcall comb (gethash k copy) v)
                                v)))
             hash)
    copy))

(defgeneric table (self))

(definline maphash-keys (function table)
  "Like MAPHASH, but calls FUNCTION with each key in the hash table TABLE."
  (maphash (lambda (k v)
             (declare (ignore v))
             (funcall function k))
           table))

(definline maphash-values (function table)
  "Like MAPHASH, but calls FUNCTION with each value in the hash table TABLE."
  (maphash (lambda (k v)
             (declare (ignore k))
             (funcall function v))
           table))

(definline hash-table-keys (table)
  "Returns a list containing the keys of hash table TABLE."
  (let ((keys nil))
    (maphash-keys (lambda (k)
                    (push k keys))
                  table)
    keys))

(definline hash-table-values (table)
  "Returns a list containing the values of hash table TABLE."
  (let ((values nil))
    (maphash-values (lambda (v)
                      (push v values))
                    table)
    values))

(definline hash-table-alist (table)
  "Returns an association list containing the keys and values of a hash-table."
  (let ((alist))
    (maphash (lambda (k v)
               (push (cons k v) alist))
             table)
    alist))

(definline hash-table-list (table)
  "Returns a list of lists containing the keys and values of TABLE."
  (let ((list))
    (maphash (lambda (k v)
               (push (list k v) list))
             table)
    list))

(definline hash-table-plist (table)
  "Returns a property list contains the keys and values of a hash-table."
  (let ((plist))
    (maphash (lambda (k v)
               (setf plist (list* k v plist)))
             table)
    plist))

(definline alist-hash-table (alist &rest hash-table-initargs)
  "Returns a hash table containing the keys and values of the association list
ALIST. Hash table is initialized using the HASH-TABLE-INITARGS."
  (let ((table (apply #'make-hash-table hash-table-initargs)))
    (dolist (cons alist)
      (ensure-gethash (car cons) table (cdr cons)))
    table))

(definline plist-hash-table (plist &rest hash-table-initargs)
  "Returns a hash table containing the keys and values of the property list
PLIST. Hash table is initialized using the HASH-TABLE-INITARGS."
  (let ((table (apply #'make-hash-table hash-table-initargs)))
    (do ((tail plist (cddr tail)))
        ((not tail))
      (ensure-gethash (car tail) table (cadr tail)))
    table))

(definline plist-string-hash-table (plist &rest hash-table-initargs)
  "Returns a hash table containing the keys and values of the property list
PLIST. Hash table is initialized using the HASH-TABLE-INITARGS."
  (let ((table (apply #'make-hash-table :test 'equal hash-table-initargs)))
    (do ((tail plist (cddr tail)))
        ((not tail))
      (ensure-gethash (string-downcase (car tail)) table (cadr tail)))
    table))

(defun pophash (key hash-table &optional default)
  (multiple-value-bind (value existsp) (gethash key hash-table default)
    (when existsp (remhash key hash-table))
    (values value existsp)))

;;; Hashers
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *global-hasher* #'sxhash))

;; TODO 2024-05-24: do better
(sb-ext:define-load-time-global *global-hash* (funcall *global-hasher* (get-universal-time)))

(macrolet ((specialize (str body) ; TODO 2023-12-21: test if this actually compiles to fastpath
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
      hash))
  (defun simple-string-hash (string)
    (declare (simple-string string)
             (optimize speed))
    (let ((value 0))
      (declare ((unsigned-byte 32) value))
      (specialize
       string
       (loop for char across string
             for position below 2
             do (setf value (logxor value (char-code char)))))
      value)))

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

;;; Concurrency

;; concurrent hash-tables

;; refs:

;; https://dspace.mit.edu/bitstream/handle/1721.1/130693/1251799942-MIT.pdf

;; https://github.com/TooBiased/growt - folklore = linear-probing, non growing hash-table

;; https://github.com/Shinmera/luckless

;; https://github.com/no-defun-allowed/luckless

;; https://github.com/telekons/42nd-at-threadmill - based on NBHM (JVM)

;; https://github.com/robert-strandh/SICL/tree/master/Code/Hash-tables/Linear-probing

;; https://github.com/no-defun-allowed/simd-sicl-hash-table

;; some CAS/Atomics resources for Linux:

;; - https://www.kernel.org/doc/html/v4.12/core-api/atomic_ops.html

;; - https://docs.kernel.org/core-api/wrappers/atomic_t.html

;; - https://www.kernel.org/doc/Documentation/memory-barriers.txt

;; - https://litux.nl/mirror/kerneldevelopment/0672327201/ch09lev1sec1.html

;; - https://docs.kernel.org/core-api/refcount-vs-atomic.html

;; - https://en.wikipedia.org/wiki/Compare-and-swap

;; - https://lwn.net/Articles/847973/

;; Notes:

;; several of the implementations above are ported in this library.

;; In general we rely on CAS operations to implement as
;; lock-free. Typically you will still need some form of thread
;; protection at higher levels of abstraction when working with these
;; type of data structures.

;; Test, test, test. We must compare every implementation and
;; benchmark their performance with real workloads.
(deftype solist-element-designator () `(member ,@(list :addr :fixnum :string)))

(defmacro make-so-set (&optional (type :addr))
  "Return a SOLIST set. Type is of type SOLIST-ELEMENT-DESIGNATOR."
  (declare (solist-element-designator type))
  `(case ,type
     (:fixnum ,(make-so-set/fixnum))
     (:string ,(make-so-map/string))
     (:addr ,(make-so-set/addr))))

(defmacro make-so-map (&optional (type :addr))
  "Return a SOLIST map. Type may be either FIXNUM or STRING."
  (declare (solist-element-designator type))
  `(case ,type
     (:fixnum ,(make-so-map/fixnum))
     (:string ,(make-so-map/string))
     (:addr ,(make-so-map/addr))))

(defun show-list (solist)
  (let ((node (so-head solist)))
    (loop (format t "~s~%" node)
          (when (endp node) (return))
          (setq node (%node-next node)))))

(defun show-bin (solist i)
  (let ((node (aref (car (so-bins solist)) i))
        (bin-nbits (- +hash-nbits+ (cdr (so-bins solist))))
        (count 0))
    (flet ((bit-string (hash)
             (let ((s (format nil " ~v,'0b" +hash-nbits+ hash)))
               (replace s s :end1 bin-nbits :start2 1)
               (setf (char s bin-nbits) #\.)
               s)))
      (cond
        ((unbound-marker-p node)
         (values 0 0))
        (t
         (let ((node node))
           (loop (let ((next (get-next node)))
                   (when (or (endp next) (evenp (node-hash next)))
                     (return))
                   (incf count)
                   (setq node next))))
         (format t " ~5d [~2d] = ~a" i count (bit-string (node-hash node)))
         (loop (let ((next (get-next node)))
                 (when (or (endp next) (evenp (node-hash next)))
                   (return))
                 (setq node next)
                 (if (= count 1)
                     (format t " ~a=~s"
                             (bit-string (node-hash node)) (so-key node))
                     (format t "~%              ~a=~s"
                             (bit-string (node-hash node)) (so-key node)))))
         (terpri)
         (values 1 count))))))

(defun show-bins (solist)
  (let ((bins (car (so-bins solist)))
        (bin-nbits (- +hash-nbits+ (cdr (so-bins solist))))
        (n-occupied-bins 0)
        (sum-chainlengths 0)
        (max-chainlength 0))
    (assert (= (length bins) (ash 1 bin-nbits)))
    (format t "Bins (~d total, ~d leading bits):~%"
            (length bins) bin-nbits)
    (dotimes (i (length bins))
      (multiple-value-bind (occupied count) (show-bin solist i)
        (incf n-occupied-bins occupied)
        (incf sum-chainlengths count)
        (setq max-chainlength (max count max-chainlength))))
    (let ((avg-chainlength (/ sum-chainlengths n-occupied-bins)))
      (format t "~&Total ~D items, avg ~F items/bin~%"
              (so-count solist) avg-chainlength)
      (values max-chainlength (float avg-chainlength)))))

(defun print-hashes (solist)
  (do ((node (%node-next (so-head solist)) (%node-next node)))
      ((endp node))
    (format t "~16x~@[ ~s~]~%"
            (node-hash node)
            (if (so-key-node-p node) (type-of (so-key node))))))
