;;; schema.lisp --- Skel Schemas

;; 

;;; Code:
(in-package :skel/core/schema)

(defclass sk-schema (rdb-schema skel) ())

(defvar *skel-registry-schema*
  (make-instance 'sk-schema
    :fields 
    (make-fields 
     :id '(fixnum . string)
     ;; blob?
     :name '(string . fixnum)
     :path '(fixnum . pathname)
     :tags '(fixnum . (array string)))))

(defvar *skel-cache-schema*
  (make-instance 'sk-schema
    :fields 
    (make-fields
     :id '(fixnum . octet-vector)
     :hash '((octet-vector 32) . fixnum)
     :updated '(fixnum . octet-vector))))

(defclass sk-object-schema (rdb-object-schema) ()
  (:default-initargs :class-name 'skel))
