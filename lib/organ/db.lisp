;;; db.lisp --- Organ Databases

;; 

;;; Code:
(in-package :organ/db)

;;; Schema
(defclass org-graph-schema (rdb-schema) ()
  (:default-initargs
   :fields (make-fields :file '(pathname . octet-vector)
                        :title '(org-id . string)
                        :hash '(org-id . string)
                        :atime '(org-id . octet-vector)
                        :mtime '(org-id . octet-vector)
                        :node '(org-id . octet-vector)
                        :edge '(org-id . octet-vector)
                        :node-tags '(org-id . string)
                        :node-links '(org-id . string)
                        :node-properties '(org-id . string)
                        :node-priority '(org-id . string)
                        :node-schedule '(org-id . string)
                        :node-file '(org-id . string)
                        :node-pos '(org-id . octet-vector)
                        :node-state '(org-id . string))))

(defvar *org-graph-schema* (make-instance 'org-graph-schema))

;;; Org Graph DB
(defvar *org-graph-db-directory* (merge-pathnames ".store/db/graph/" (user-homedir-pathname)))

(defun make-org-graph-db ()
  (load-schema
   (make-db :rdb :path (namestring *org-graph-db-directory*)
            :opts (default-rocksdb-options))
   *org-graph-schema*))

(defvar *org-graph-db* nil)

(defun close-org-graph-db ()
  (when (db-open-p *org-graph-db*)
    (shutdown-db *org-graph-db* :wait t)
    (close-db *org-graph-db*)))

(defun init-org-graph-db ()
  (ensure-directories-exist
   (make-pathname :directory (butlast (pathname-directory *org-graph-db-directory*)))
   :verbose t)
  (open-org-graph-db)
  (make-thread 'insert-org-files)
  (make-thread 'insert-org-nodes)
  (make-thread 'insert-org-edges)
  (log:info! "created org-graph-db" *org-graph-db* *org-graph-db-directory* *org-graph-schema*))

(defmethod init ((self (eql :org-graph-db)) &key)
  (init-org-graph-db))

(defun open-org-graph-db ()
  (unless *org-graph* (init-org-graph))
  (if (and *org-graph-db* (db-open-p *org-graph-db*))
      *org-graph-db*
      (progn
        (setq *org-graph-db* (make-org-graph-db))
        (push-opts *org-graph-db*)
        (open-columns* *org-graph-db*))))

(defun destroy-org-graph-db ()
  (unless (db-closed-p *org-graph-db*)
    (close-db *org-graph-db*))
  (destroy-db *org-graph-db*)
  (log:info! "destroyed org-graph-db at ~A" *org-graph-db-directory*))

(defun og-get (key &optional (from "node"))
  (get-val *org-graph-db* key :data-type 'string :column from))

(defun og-values (column)
  (with-iter (it (iter *org-graph-db* :column (find-column column *org-graph-db*)))
    seek-to-first
    (loop while iter-valid-p
          collect (cons (handler-case (octet-vector-to-uuid key)
                          (simple-type-error () (sb-ext:octets-to-string key)))
                        (when (zerop (mod (length val) 16))
                          (loop for i from 0 below (length val) by 16
                                collect (octet-vector-to-uuid (subseq val i (+ i 16))))))
          do (progn next))))

(defun insert-org-files ()
  (log:info! "inserting org files")
  (mapc
   (lambda (n) 
     (insert-key *org-graph-db* 
                 (namestring (path n))
                 (apply 'concatenate 'vector
                        (mapcar 'uuid-to-octet-vector
                                (flatten
                                 (map 'list (lambda (x) 
                                              (when-let ((i (id x)))
                                                (make-uuid-from-string i)))
                                      (ast n)))))
                 :column "file"))
   *org-graph-files*))

(defun insert-org-nodes ()
  (log:info! "inserting org nodes")
  (dolist (id (mapcar 'id *org-graph-nodes*))
    (insert-key *org-graph-db*
                (uuid-octets* id)
                ;; TODO 2024-12-30: 
                #(1)
                :column "node")))

(defun insert-org-edges ()
  (log:info! "inserting org edges")
  (dolist (e *org-graph-edges*)
    (insert-key *org-graph-db*
                (uuid-octets* (edge-in e)) 
                (uuid-octets* (edge-out e))
                :column "edge")))
