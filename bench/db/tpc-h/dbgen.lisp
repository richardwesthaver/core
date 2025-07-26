;;; dbgen.lisp --- TPC-H DBGEN

;; This package contains an implementation of the TPCH-DBGEN program.

;;; Commentary:

;; ref: https://www.tpc.org/tpc_documents_current_versions/pdf/tpc-h_v2.17.1.pdf

;; ref: https://github.com/electrum/tpch-dbgen

;; The TPCH-DBGEN source generates ASCII pipe-delimited output and it seems
;; pretty common to roll your own implementation. For full compliance we are
;; supposed to generate output that is EXACTLY the same as the output as the
;; original tool, but in practice we may skip this and ingest data directly
;; into the database. We'll see. For now we aspire to generate ASCII.

#|
DBGEN is a database population program for use with the TPC-H benchmark.  
It is written in ANSI 'C' for portability, and has 
been successfully ported to over a dozen different systems. While the 
TPC-H specification allow an implementor to use any utility 
to populate the benchmark database, the resultant population must exactly 
match the output of DBGEN. The source code has been provided to make the 
process of building a compliant database population as simple as possible.
|#

;;; Code:
(defpackage :core/bench/dbgen                          
  (:nicknames :bench/dbgen :dbgen)                     
  (:import-from :obj/time :date)                       
  (:import-from :tpc-h                                 
   :+tpc-h-region-count+ :+tpc-h-nation-count+         
   :*tpc-h-data-directory*
   :write-nation-table :write-region-table             
   :write-part-table :write-supplier-table             
   :write-partsupp-table :write-customer-table         
   :write-orders-table :write-lineitem-table           
   :write-nation-row :write-region-row                 
   :write-part-row :write-supplier-row                 
   :write-partsupp-row :write-customer-row             
   :write-orders-row :write-lineitem-row)              
  (:use :cl :std :log :dat/csv :dat/proto :obj/schema) 
  (:export :dbgen))
(in-package :core/bench/dbgen)
(in-readtable :core)
(defgeneric gen-table (self count))

(defun random-id32 () (octets-to-integer (random-bytes 4)))
(defun random-id64 () (octets-to-integer (random-bytes 8)))
(defun random-string (&optional (n 25)) (random-chars n))
(defun random-date () (obj/time:today))
  
(defun random-double () ;; [0,10000)
  (coerce (* (random 100.0) 100) 'double-float))

(defun make-random-value (type)
  (cond
    ((equal '(unsigned-byte 32) type) (random-id32))
    ((equal '(unsigned-byte 64) type) (random-id64))
    ((eql 'double-float type) (random-double))
    ((eql 'date type) (random-date))
    ((eql 'character type) (random-char))
    ((and (consp type) (eql (car type) 'string)) (random-string (cdr type)))
    (t (error 'invalid-argument :reason "Invalid TPC-H type designator" :item type))))

(defun dbgen-kernel ()
  (lambda (x y)
    (gen-table x y)
    (std/thread:print-top-level (format nil "finished: ~A~%" x))))

(declaim (inline dbgen))
(defun dbgen (&optional (scale-factor 1)) ;; ~= 2.4G, 200s
  "Generate the TPC-H database in standardized format (|-delim ASCII). Files are
written with a .tbl extension to *TPC-H-DATA-DIRECTORY*."
  (declare (optimize (speed 3) (safety 0)) (fixnum scale-factor))
  (let ((region-count +tpc-h-region-count+)
        (nation-count +tpc-h-nation-count+)
        (part-count (* scale-factor 200000))
        (supplier-count (* scale-factor 10000))
        (partsupp-count (* scale-factor 800000))
        (customer-count (* scale-factor 150000))
        (lineitem-count (* scale-factor 6000000))
        (order-count (* scale-factor 1500000)))
    (declare (fixnum part-count supplier-count partsupp-count customer-count lineitem-count order-count))
    (info! "Generating new TPC-H database:" *tpc-h-data-directory*)
    (debug! (format nil "scale-factor=~A~%" scale-factor))
    (rt:is
     (wait-for-threads
      (loop for args in `((:region ,region-count)
                          (:nation ,nation-count)
                          (:part ,part-count)
                          (:supplier ,supplier-count)
                          (:partsupp ,partsupp-count)
                          (:customer ,customer-count)
                          (:lineitem ,lineitem-count)
                          (:orders ,order-count))
            collect (make-thread (dbgen-kernel) :name (string-downcase (symbol-name (car args)))
                                                :arguments args))))))

(defun tpc-h-benchmark () (dbgen:dbgen))
;; (length (read-orders-table))
;; (make-region-table-batch #(1 2 3))
;; (write-region-row :regionkey 0 :name "USA" :comment "OORAH")
;; (gen-table :orders 100000)

;; (deftest dbgen (:profile t :bench t #+nil :args #+nil (&optional (scale 1))) (dbgen))
