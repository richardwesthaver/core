;;; lib/obj/hash/set.lisp --- HashSets

;;

;;; Code:
(in-package :obj/hash)

;;; SOLIST

;; Lockfree hashsets

;; see file src/code/solist.lisp

(defmacro make-so-set (&optional (type :addr))
  "Return a SOLIST set. Type is of type SOLIST-ELEMENT-DESIGNATOR."
  (declare (solist-element-designator type))
  `(case ,type
     (:fixnum ,(make-so-set/fixnum))
     (:string ,(make-so-map/string))
     (:addr ,(make-so-set/addr))))
