;;; condition.lisp --- Rustls Conditions

;; 

;;; Code:
(in-package :rustls)

(define-condition rustls-condition () ())

(define-condition rustls-c-error (rustls-condition std:std-error) ())

(defun rustls-c-error (code)
  (let ((ret (rustls-result* code)))
    (unless (eql :ok ret)
      (error 'rustls-c-error :message (format nil "Rustls signaled an error: ~A (~A)" ret code)))))
