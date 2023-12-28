(in-package :organ)

(define-condition organ-error (error) 
  ((message :initarg :message
            :reader organ-error-message))
  (:documentation "Error signaled by the ORGAN system"))

(defmethod print-object ((obj organ-error) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a" (organ-error-message obj))))

(define-condition org-file-error (organ-error)
  ((path :initarg :path
         :reader organ-error-path))
  (:documentation "Error signaled from a missing org file path."))

(defun org-file-missing (path)
  (error 'org-file-error :message "Org file does not exist"
                         :path path))

(define-condition org-parse-error (parse-error organ-error) ())

(define-condition org-write-error (stream-error organ-error) ())
