;;; rdb/condition.lisp --- RDB conditions

;;

;;; Code:
(in-package :rdb)

(eval-always
  (deferror simple-rdb-error (simple-error db-condition)
    ()
    (:reporter t)
    (:documentation "Simple RDB Error."))
  (deferror rdb-error (db-error)
    ()
    (:reporter t)
    (:documentation "Error signaled by the RDB system."))
  (defwarning simple-rdb-warning (simple-warning db-condition)
    () 
    (:default-initargs 
     :format-control "RDB warning: ~A")
    (:reporter t)))

(defwarning rdb-default-column-warning (simple-rdb-warning simple-warning) () (:reporter t))

(define-condition rdb-alien-error (rdb-error rocksdb-c-error)
  ((db :initarg :db :reader error-db))
  (:documentation "Error signaled by RDB C subsystem."))

(defmethod print-object ((obj rdb-error) stream)
  (print-unreadable-object (obj stream :type t :identity t)))

(define-condition open-db-error (rdb-alien-error)
  ()
  (:documentation "Error signaled while opening a database."))

(define-condition destroy-db-error (rdb-alien-error)
  ()
  (:documentation "Error signaled while destroying a database."))

(define-condition flush-db-error (rdb-alien-error)
  ()
  (:documentation "Error signaled while flushing a database."))

(define-condition ingest-db-error (rdb-alien-error)
  ()
  (:documentation "Error signaled while ingesting a database."))

(define-condition sst-writer-error (rdb-alien-error)
  ()
  (:documentation "Error signaled while writing a SST file."))

(define-condition repair-db-error (rdb-alien-error)
  ()
  (:documentation "Error signaled while repairing a database."))

(define-condition destroy-backup-engine-error (rdb-alien-error)
  ()
  (:documentation "Error signaled while destroying a backup engine."))

(define-condition cf-error (rdb-alien-error)
  ((cf :initarg :cf :reader error-cf))
  (:documentation "Error signaled in the context of a Column Family."))

(define-condition kv-error (rdb-error)
  ((kv :initarg :kv :reader error-kv)))

(define-condition kv-cf-error (cf-error)
  ((kv :initarg :kv :reader error-kv)))

(define-condition put-kv-error (kv-error) ()
  (:documentation "Error signaled while processing a PUT-KV request"))

(define-condition put-kv-cf-error (kv-cf-error) ()
  (:documentation "Error signaled while processing a PUT-KV-CF request"))

(define-condition get-kv-error (kv-error) ()
  (:documentation "Error signaled while processing a GET-KV request"))

(define-condition get-kv-cf-error (kv-error) ()
  (:documentation "Error signaled while processing a GET-KV-CF request"))

(define-condition merge-kv-error (kv-error) ()
  (:documentation "Error signaled while processing a MERGE-KV request"))

(define-condition merge-kv-cf-error (kv-error) ()
  (:documentation "Error signaled while processing a MERGE-KV-CF request"))

(define-condition opt-handler-missing (warning)
  ())

(define-condition db-missing (warning)
  ())

(define-condition metadata-missing (warning)
  ())

(define-condition invalid-propname (rdb-error)
  ()
  (:documentation "Error signaled when an invalid ROCKSDB-PROPERTY value is detected."))

(define-condition rdb-transaction-error (rdb-alien-error transaction-error)
  ((txn :initarg :txn :reader error-txn))
  (:documentation "Error signaled in the context of a Transaction."))
