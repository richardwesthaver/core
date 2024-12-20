;;; mpk/db.lisp --- Media Database

;; 

;;; Code:
(in-package :mpk/db)

(defvar *mdb-directory* (mpk-path "db/"))

(defvar *mdb* nil)
(load-database-backend :rdb)

(defvar *mdb-id-seed* (random 99999))

(defclass mdb-id (id:id) ()
  (:default-initargs :id *mdb-id-seed*))

(defmethod id:make-id ((self (eql :mdb))) (make-instance 'mdb-id :id *mdb-id-seed*))

(defvar *mdb-schema* (make-instance 'rdb-schema
                       :fields (make-fields 
                                :id '(word . string)
                                :file '(word . string)
                                :name '(word . string)
                                :source '(word . string)
                                :state '(word . octet))))

(defun mdb-init ()
  (ifret *mdb*
    (setq *mdb* 
          (make-db :rdb 
                   :opts (default-rdb-opts)
                   :name (namestring *mdb-directory*)))
    (if (probe-file *mdb-directory*)
        (progn
          (load-opts *mdb* :backfill t)
          (open-db *mdb*))
        (progn 
          (open-db *mdb*)
          (load-schema *mdb* *mdb-schema*)
          (create-columns *mdb*)
          ;; (sync-db *mdb* nil :wait t)
          (close-db *mdb*)))))

;; (setq *mdb* nil)
;; (mdb-init)
;; (columns *mdb*)
;; (create-columns *mdb*)
;; (open-columns* *mdb*)
