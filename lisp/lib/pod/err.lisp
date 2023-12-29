;;; lib/pod/err.lisp --- Pod Errors

;;

;;; Code:
(in-package :pod)

(define-condition pod-error (error) ())

(define-condition podman-error (pod-error) ())

(defun handle-podman-error (err))
