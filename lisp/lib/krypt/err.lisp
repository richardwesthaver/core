;;; krypt/err.lisp --- Krypt Errors

;;

;;; Code:
(in-package :krypt)

(define-condition krypt-error (error)
  ()
  (:documentation "Error signaled by the KRYPT system."))

(deferror krypt-simple-error (krypt-error simple-error) () (:auto t))

(defun krypt-simple-error (ctrl &rest args)
  (error 'simple-sql-error :format-control ctrl :format-arguments args))
