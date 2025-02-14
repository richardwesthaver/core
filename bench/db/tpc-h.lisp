;;; tpc-h.lisp --- TPC-H Benchmark Suite

;; This package contains an implementation of the TPC-H benchmark.

;;; Commentary:

;; ref: https://www.tpc.org/tpc_documents_current_versions/pdf/tpc-h_v2.17.1.pdf

;; The TPC-H dbgen source is out there somewhere. It generates ASCII
;; pipe-delimited output and it seems pretty common to roll your own
;; implementation. For full compliance we are supposed to generate output that
;; is EXACTLY the same as the output as the original tool, but in practice we
;; may skip this and ingest data directly into the database. We'll see. For
;; now we aspire to generate ASCII.

;;; Code:
(defpackage :core/bench/tpc-h
  (:nicknames :bench/tpc-h :tpc-h)
  (:import-from :obj/time :date)
  (:import-from :cli/clap :defmain)
  (:use :cl :std :rt :rt/bench :rt/cover :log :sql :parse/pratt :dat/csv :dat/proto :obj/query :obj/schema)
  (:export :tpc-h-schema :*tpc-h-data-directory*
           :start-tpc-h-benchmark))

(in-package :core/bench/tpc-h)

(defsuite :tpc-h)
(in-suite :tpc-h)

(eval-always
  (declaim (pathname *tpc-h-data-directory*))
  (defvar *tpc-h-data-directory* 
    (ensure-directories-exist (directory-path (translate-logical-pathname "core:tmp;tpc-h")))))

(defclass tpc-h-schema (schema) ())

(defmethod apply-schema ((self tpc-h-schema) (object t))
  (let ((flen (length (fields self)))
        (olen (length object)))
    (unless (= flen olen)
      (error 'invalid-argument :reason "Field count doesn't match length of object" :item object))))

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

(defun dbgen-thread ()
  (lambda (x y)
    (gen-table x y)
    (std/thread:print-top-level (format nil "finished: ~A~%" x))))

(defun dbgen (&optional (scale-factor 1)) ;; ~= 2.4G, 200s
  "Generate the TPC-H database in standardized format (|-delim ASCII). Files are
written with a .tbl extension to *TPC-H-DATA-DIRECTORY*."
  (let ((region-count +tpc-h-region-count+)
        (nation-count +tpc-h-nation-count+)
        (part-count (* scale-factor 200000))
        (supplier-count (* scale-factor 10000))
        (partsupp-count (* scale-factor 800000))
        (customer-count (* scale-factor 150000))
        (lineitem-count (* scale-factor 6000000))
        (order-count (* scale-factor 1500000)))
    (info! "Generating new TPC-H database:" *tpc-h-data-directory*)
    (debug! (format nil "scale-factor=~A~%" scale-factor))
    (assert
     (wait-for-threads
      (loop for args in `((:region ,region-count)
                          (:nation ,nation-count)
                          (:part ,part-count)
                          (:supplier ,supplier-count)
                          (:partsupp ,partsupp-count)
                          (:customer ,customer-count)
                          (:lineitem ,lineitem-count)
                          (:orders ,order-count))
            collect (make-thread (dbgen-thread) :name (string-downcase (symbol-name (car args)))
                                                :arguments args))))))

(defmain start-tpc-h-benchmark (:exit nil)
  (dbgen))

;; (length (read-orders-table))
;; (make-region-table-batch #(1 2 3))
;; (write-region-row :regionkey 0 :name "USA" :comment "OORAH")
;; (gen-table :orders 100000)

;; (deftest dbgen (:profile t :bench t #+nil :args #+nil (&optional (scale 1))) (dbgen))
