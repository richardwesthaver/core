;;; lib/obj/hash/chash.lisp --- Concurrent Hashing

;;

;;; Notes:

;; https://dspace.mit.edu/bitstream/handle/1721.1/130693/1251799942-MIT.pdf

;; https://github.com/Shinmera/luckless

;; https://github.com/telekons/42nd-at-threadmill - based on NBHM (JVM)

;; https://github.com/robert-strandh/SICL/tree/master/Code/Hash-tables/Linear-probing

;; https://github.com/no-defun-allowed/simd-sicl-hash-table

;;; Code:
(in-package :obj/hash)
