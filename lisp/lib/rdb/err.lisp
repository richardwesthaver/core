(in-package :rdb)

(define-condition rdb-error (error)
  ()
  (:documentation "Error signaled by the RDB system"))

(defmethod print-object ((obj rdb-error) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "error-message=~A" (error-message obj))))

(define-condition open-db-error (rdb-error)
  ((db-path :initarg :db-path
            :reader db-path)
   (error-message :initarg :error-message
                  :reader error-message))
  (:documentation "Error signaled while opening a db"))

(define-condition put-kv-error (rdb-error)
  ((db :initarg :db
       :reader db)
   (key :initarg :key
        :reader key)
   (val :initarg :val
        :reader val)
   (error-message :initarg :error-message
                  :reader error-message))
  (:documentation "Error signaled while processing a PUT-KV request"))

(define-condition get-kv-error (rdb-error)
  ((db :initarg :db
       :reader db)
   (key :initarg :key
        :reader key)
   (error-message :initarg :error-message
                  :reader error-message))
  (:documentation "Error signaled while processing a GET-KV request"))
