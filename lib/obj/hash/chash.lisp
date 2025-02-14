;;; lib/obj/hash/chash.lisp --- Concurrent Hash Tables

;; concurrent hash-tables

;; API compatible with:
;; https://github.com/no-defun-allowed/concurrent-hash-tables

;;; Resources:

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

;;; Notes:

;; several of the implementations above are ported in this library.

;; In general we rely on CAS operations to implement as
;; lock-free. Typically you will still need some form of thread
;; protection at higher levels of abstraction when working with these
;; type of data structures.

;; Test, test, test. We must compare every implementation and
;; benchmark their performance with real workloads.

;;; Code:
(in-package :obj/hash)

(deftype solist-element-designator () `(member ,@(list :addr :fixnum :string)))

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
(sb-lockless:lfl-insert (sb-lockless:make-ordered-list :key-type 'fixnum) 5 'five)
