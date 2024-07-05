;;; tpc-h.lisp --- TPC-H Benchmark Suite

;; This package contains an implementation of the TPC-H benchmark.

;;; Commentary:

;; ref: https://www.tpc.org/tpc_documents_current_versions/pdf/tpc-h_v2.17.1.pdf

;;; Code:
(defpackage :core/bench/tpc-h
  (:nicknames :bench/tpc-h :tpc-h)
  (:import-from :obj/query :make-field)
  (:import-from :obj/time :date)
  (:use :cl :std :rt/bench :rt/cover :log :sql :parse/pratt :dat/csv :dat/proto :obj/query))

(in-package :core/bench/tpc-h)

(declaim (pathname *tpc-h-data-directory*))
(defvar *tpc-h-data-directory* (ensure-directories-exist #p"/tmp/tpc-h/"))

;;; Dbgen

;; The TPC-H dbgen source is out there somewhere. It generates ASCII
;; pipe-delimited output and it seems pretty common to roll your own
;; implementation. For full compliance we are supposed to generate output that
;; is EXACTLY the same as the output as the original tool, but in practice we
;; may skip this and ingest data directly into the database. We'll see. For
;; now we aspire to generate ASCII.

(defclass tpc-h-schema (schema) ())

(defun parse-tpc-h-fields (fields)
  (let ((ret))
    (sb-int:doplist (k v) fields
      (push (make-field :name (string-downcase (symbol-name k)) :type v)
            ret))
    ret))

(defmacro def-table (name &rest fields)
  "Define a new 'NAME.tbl' file in *TPC-H-DATA-DIRECTORY*. DATA is a vector of
TABLE-ROW objects."
  (with-gensyms (data)
    (let ((path (merge-pathnames
                 (make-pathname :name (string-downcase (symbol-name name))
                                :type "tbl"
                                :directory nil)
                 *tpc-h-data-directory*)))
      `(progn
         (defclass ,(symbolicate name '-table-schema) (tpc-h-schema) ()
           (:default-initargs
            :fields ,(coerce (parse-tpc-h-fields fields) 'vector)))
         (defun ,(symbolicate 'write- name '-table) (,data)
           (write-csv-file ,path ,data
                           :delimiter #\|))
         (defparameter ,(symbolicate '* name '-table-path*) ,path)
         (defun ,(symbolicate 'read- name '-table) ()
           (read-csv-file ,path :delimiter #\| :header nil))))))

;;;; Schemas

;; nation
(def-table nation
  :nationkey '(unsigned 32)
  :name '(string 25)
  :regionkey '(unsigned 32)
  :comment '(string 152))
  
;; region
(def-table region
  :regionkey '(unsigned 32)
  :name '(string 25)
  :comment '(string 152))

;; part
(def-table part
  :partkey '(unsigned 64)
  :name '(string 55)
  :mfgr '(string 25)
  :brand '(string 10)
  :type '(string 25)
  :size '(unsigned 32)
  :container '(string 10)
  :retailprice 'double-float
  :comment '(string 23))

;; supplier
(def-table supplier
  :suppkey '(unsigned 64)
  :name '(string 25)
  :address '(string 40)
  :nationkey '(unsigned 32)
  :phone '(string 15)
  :acctbal 'double-float
  :comment '(string 101))

;; partsupp
(def-table partsupp
  :partkey '(unsigned 64)
  :suppkey '(unsigned 64)
  :availqty '(unsigned 64)
  :supplycost 'double-float
  :comment '(comment 199))

;; customer
(def-table customer
  :custkey '(unsigned 64)
  :name '(string 25)
  :address '(string 40)
  :nationkey '(unsigned 32)
  :phone '(string 15)
  :acctbal 'double-float
  :mktsegment '(string 10)
  :comment '(string 117))

;; orders
(def-table orders
  :orderkey '(unsigned 64)
  :custkey '(unsigned 64)
  :orderstatus 'character
  :totalprice 'double-float
  :orderdate 'date
  :orderpriority '(string 15)
  :clerk '(string 15)
  :shippriority '(unsigned 32)
  :comment '(string 79))

;; lineitem
(def-table lineitem
  :orderkey '(unsigned 64)
  :partkey '(unsigned 64)
  :suppkey '(unsigned 64)
  :linenumber '(unsigned 64)
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

(defun dbgen (&optional (scale 1))
  "Generate the TPC-H database in standardized format (|-delim ASCII). Files are
written with a .tbl extension to *TPC-H-DATA-DIRECTORY*.")
