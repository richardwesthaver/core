;;; mpk.lisp --- Media Production Kit

;; 

;;; Code:
(in-package :mpk)

(defun init-mpk (&key (db t))
  (load-mpkrc)
  (load-av)
  (when db (db:load-database-backend :mpk))
  (setf *thread-pool* (make-thread-pool (num-cpus) :name :mpk)))

(defmethod init ((self (eql :mpk)) &key db)
  (init-mpk :db db))
