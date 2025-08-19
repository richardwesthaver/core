;;; proto.lisp --- TPC-H Protocols

;; 

;;; Code:
(defpackage :core/bench/tpc-h                 
  (:nicknames :bench/tpc-h :tpc-h)            
  (:import-from :obj/time :date)              
  (:use :cl :std :rt :log :schema)
  (:export :*tpc-h-data-directory*            
   :tpc-h-schema :tpc-h-benchmark             
   :+tpc-h-region-count+ :+tpc-h-nation-count+
   :read-nation-table :read-region-table      
   :read-part-table :read-supplier-table      
   :read-partsupp-table :read-customer-table  
   :read-orders-table :read-lineitem-table))  

(in-package :tpc-h)
(in-readtable :std)
(eval-always
  (declaim (pathname *tpc-h-data-directory*))
  (defvar *tpc-h-data-directory* 
    (ensure-directories-exist (directory-path #l"sys:tmp;tpc-h"))))

(defclass tpc-h-schema (schema) ())

(defmethod apply-schema ((self tpc-h-schema) (object t))
  (let ((flen (length (fields self)))
        (olen (length object)))
    (unless (= flen olen)
      (error 'invalid-argument :reason "Field count doesn't match length of object" :item object))))

(eval-always
  (defun parse-tpc-h-fields (fields)
    (let ((ret) (keys) (val-forms))
      (sb-int:doplist (k v) fields
        (push (make-field :name (string-downcase (symbol-name k)) :type v)
              ret)
        (push (symbolicate k) keys)
        (push k val-forms)
        (push `(make-random-value ,v) val-forms))
      (values (coerce (nreverse ret) '(vector field)) (nreverse keys) (nreverse val-forms)))))

(defmacro def-table (name &rest fields)
  "Define a new TPC-H table."
  (with-gensyms (data)
    (let ((path (merge-pathnames
                 (make-pathname :name (string-downcase (symbol-name name))
                                :type "tbl"
                                :directory nil)
                 *tpc-h-data-directory*))
          (schema-class (symbolicate name '-table-schema))
          (write-tbl-fn (symbolicate 'write- name '-table))
          (write-row-fn (symbolicate 'write- name '-row))
          (make-batch-fn (symbolicate 'make- name '-table-batch))
          (path-var (symbolicate '* name '-table-path*))
          (read-tbl-fn (symbolicate 'read- name '-table)))
      (multiple-value-bind (field-vec keys val-forms) (parse-tpc-h-fields fields)
        `(progn
         (defclass ,schema-class (tpc-h-schema) ()
           (:default-initargs
            :fields ,field-vec))
         (defmethod apply-schema ((self ,schema-class) (object t))
           (let ((flen (length (fields self)))
                 (olen (length object)))
             (unless (= flen olen)
               (error 'invalid-argument :reason "Field count doesn't match length of object" :item object))))
         (defmethod gen-table ((self (eql ,(keywordicate name))) (count fixnum))
           (declare (ignore self))
           (loop for i below count
                 do (,write-row-fn
                     ,@val-forms)))
         (defun ,write-tbl-fn (,data)
           (apply-schema (make-instance ',schema-class) ,data)
             (write-csv-file ,path ,data
                             :delimiter #\|))
         (defun ,write-row-fn (&key ,@keys)
           (let ((,data (vector ,@keys)))
             (apply-schema (make-instance ',schema-class) ,data)
           (with-open-file (file ,path :direction :output :if-exists :append :if-does-not-exist :create)
             (write-csv-stream file (vector ,data) :delimiter #\|))))
         (defun ,make-batch-fn (,data)
           (let ((schema (make-instance ',schema-class)))
             (apply-schema schema ,data)
             (make-record-batch :schema schema :fields ,data)))
         (defparameter ,path-var ,path)
         (declaim (inline ,read-tbl-fn))
         (defun ,read-tbl-fn ()
           (read-csv-file ,path :delimiter #\| :header nil)))))))

;; nation
(def-table nation
  :nationkey '(unsigned-byte 32)
  :name '(string 25)
  :regionkey '(unsigned-byte 32)
  :comment '(string 152))

;; region
(def-table region
  :regionkey '(unsigned-byte 32)
  :name '(string 25)
  :comment '(string 152))

;; part
(def-table part
  :partkey '(unsigned-byte 64)
  :name '(string 55)
  :mfgr '(string 25)
  :brand '(string 10)
  :type '(string 25)
  :size '(unsigned-byte 32)
  :container '(string 10)
  :retailprice 'double-float
  :comment '(string 23))

;; supplier
(def-table supplier
  :suppkey '(unsigned-byte 64)
  :name '(string 25)
  :address '(string 40)
  :nationkey '(unsigned-byte 32)
  :phone '(string 15)
  :acctbal 'double-float
  :comment '(string 101))

;; partsupp
(def-table partsupp
  :partkey '(unsigned-byte 64)
  :suppkey '(unsigned-byte 64)
  :availqty '(unsigned-byte 64)
  :supplycost 'double-float
  :comment '(string 199))

;; customer
(def-table customer
  :custkey '(unsigned-byte 64)
  :name '(string 25)
  :address '(string 40)
  :nationkey '(unsigned-byte 32)
  :phone '(string 15)
  :acctbal 'double-float
  :mktsegment '(string 10)
  :comment '(string 117))

;; orders
(def-table orders
  :orderkey '(unsigned-byte 64)
  :custkey '(unsigned-byte 64)
  :orderstatus 'character
  :totalprice 'double-float
  :orderdate 'date
  :orderpriority '(string 15)
  :clerk '(string 15)
  :shippriority '(unsigned-byte 32)
  :comment '(string 79))

;; lineitem
(def-table lineitem
  :orderkey '(unsigned-byte 64)
  :partkey '(unsigned-byte 64)
  :suppkey '(unsigned-byte 64)
  :linenumber '(unsigned-byte 64)
  :quantity 'double-float
  :extendedprice 'double-float
  :discount 'double-float
  :tax 'double-float
  :returnflag 'character
  :linestatus 'character
  :shipdate 'date
  :receiptdate 'date
  :shipinstruct '(string 25)
  :shipmode '(string 10)
  :comment '(string 44))

(defconstant +tpc-h-region-count+ 5)
(defconstant +tpc-h-nation-count+ 25)
