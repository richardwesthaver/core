;;; tpc-h.lisp --- TPC-H DBGEN

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
(in-package :tpc-h)
(defsuite :tpc-h)
(in-suite :tpc-h)
(defun tpc-h-benchmark () (dbgen))
;; (length (read-orders-table))
;; (make-region-table-batch #(1 2 3))
;; (write-region-row :regionkey 0 :name "USA" :comment "OORAH")
;; (gen-table :orders 100000)

;; (deftest dbgen (:profile t :bench t #+nil :args #+nil (&optional (scale 1))) (dbgen))
