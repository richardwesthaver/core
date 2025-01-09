;;; schema.lisp --- Skel Schemas

;; 

;;; Code:
(in-package :skel/db)

(defclass sk-schema (rdb-schema skel) ())
(defclass sk-log-schema (rdb-log-schema skel) ())

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

(defvar *skel-log-schema* (make-instance 'sk-log-schema))

(defclass sk-object-schema (rdb-object-schema) ()
  (:default-initargs :class-name 'skel))
