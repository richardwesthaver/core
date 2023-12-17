;;; lib/obj/hash.lisp --- Hash Table Primitives

;; ANSI CL provides hash-tables, SBCL allows extending them. This
;; package builds on the SBCL extension interface.

;; - SXHASH is the built-in ANSI CL hash function

;;; Code:
(in-package :obj/hash)

(defgeneric hash-object (obj)
  (:documentation "Return the hash of OBJ.")
  (:method ((obj t)) (sxhash obj)))

;;; Hash Set

;;;; Robinhood
;; support for SBCL's robinhood-hashing weak hashset

;; ref: https://cs.uwaterloo.ca/research/tr/1986/CS-86-14.pdf
;; concurrent implementation: https://dspace.mit.edu/bitstream/handle/1721.1/130693/1251799942-MIT.pdf

;; (sb-impl:make-hashset,hashset-remove,hashset-statistics)
