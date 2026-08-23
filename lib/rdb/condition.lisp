;;; rdb/condition.lisp --- RDB conditions

;;

;;; Code:
(in-package :rdb)

(eval-always
  (deferror simple-rdb-error (simple-error db-condition)
    ()
    (:reporter t)
    (:documentation "Simple RDB Error."))
  (defwarning simple-rdb-warning (simple-warning db-condition)
    () 
    (:default-initargs 
     :format-control "RDB warning: ~A")
    (:reporter t)))

(defwarning rdb-default-column-warning (simple-rdb-warning simple-warning) () (:reporter t))

(define-condition rdb-alien-error (db-error rocksdb-c-error)
  ((db :initarg :db :reader error-db :initform *db*))
  (:report (lambda (c s) (format s "Error in DB ~A: ~A" (error-db c) (error-message c))))
  (:documentation "Error signaled by RDB C subsystem."))

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

(define-condition kv-error (rdb-alien-error)
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

(define-condition opt-handler-missing (db-warning)
  ())

(define-condition db-missing (db-warning)
  ())

(define-condition metadata-missing (db-warning)
  ())

(define-condition invalid-propname (db-error)
  ()
  (:documentation "Error signaled when an invalid ROCKSDB-PROPERTY value is detected."))
