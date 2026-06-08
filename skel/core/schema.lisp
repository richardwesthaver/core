;;; schema.lisp --- Skel Schemas

;; 

;;; Code:
(in-package :skel/core)

(defclass skel-schema (rdb-schema skel) ())

(defvar *skel-registry-schema*
  (make-instance 'skel-schema
    :fields 
    (make-fields 
     :id '(fixnum . string)
     :properties '(fixnum . octet-vector)
     :name '(string . fixnum)
     :path '(fixnum . pathname)
     :tags '(fixnum . (array string)))))

(defvar *skel-cache-schema*
  (make-instance 'skel-schema
    :fields 
    (make-fields
     :id '(fixnum . octet-vector)
     :hash '((octet-vector 32) . fixnum)
     :updated '(fixnum . octet-vector))))

(defschema skel-object-schema (rdb-object-schema) ()
  (:default-initargs :class-name 'skel))
