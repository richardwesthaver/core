;;; mpk.lisp --- Media Production Kit

;; 

;;; Code:
(in-package :mpk)

(defun mpk-init (&key (db t))
  (load-mpkrc)
  (load-av)
  (when db (db:load-database-backend :mpk))
  (setf *thread-pool* (make-thread-pool (num-cpus) :name :mpk)))
