;;; lib/obj/hash/map.lisp --- HashMaps

;;

;;; Code:
(in-package :obj/hash)

;;; SOLIST

;; Lockfree Maps

(defmacro make-so-map (&optional (type :addr))
  "Return a SOLIST map. Type may be either FIXNUM or STRING."
  (declare (solist-element-designator type))
  `(case ,type
     (:fixnum ,(make-so-map/fixnum))
     (:string ,(make-so-map/string))
     (:addr ,(make-so-map/addr))))
