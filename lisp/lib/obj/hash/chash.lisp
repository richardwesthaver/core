;;; lib/obj/hash/chash.lisp --- Concurrent Hashing

;; chash-tables

;;; Resources:

;; https://dspace.mit.edu/bitstream/handle/1721.1/130693/1251799942-MIT.pdf

;; https://github.com/TooBiased/growt - folklore = linear-probing, non growing hash-table

;; https://github.com/Shinmera/luckless

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
