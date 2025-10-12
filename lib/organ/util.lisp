;;; lib/organ/util.lisp --- Organ Utils

;;

;;; Code:
(in-package :organ)

(defun read-org-lines (&optional stream)
  (apply #'vector
	 (loop for l = (read-line stream nil)
	       until (not l)
	       collect l)))

(defun read-org-lines-from-string (str)
  (with-input-from-string (s str) (read-org-lines s)))

;; (sym-to-org-class-name 'headline)
(eval-always
(defun sym-to-org-class-name (sym) 
  "Convert keyword or symbol SYM to a symbol which could designate an ORG- object type."
  (intern (format nil "~:@(~a~a~)" "org-" sym) :organ)))

;;; Org IDs
(deftype org-id () `(octet-vector 16))

(define-condition org-id-locations-out-of-sync (simple-error) ())
(defvar *org-id-locations-file* (merge-pathnames ".emacs.d/.org-id-locations" (user-homedir-pathname)))
(defun make-org-id-locations (&optional (file *org-id-locations-file*))
  (let ((tbl (make-hash-table :test 'equal)))
    (with-open-file (file file)
      (dolist (entry (read file))
        (if-let ((file (probe-file (car entry))))
          (setf (gethash (namestring file) tbl) (cdr entry))
          (signal 'org-id-locations-out-of-sync :format-control "~A" :format-arguments (list entry)))))
    tbl))

(defun uuid-octets* (id)
  (handler-case (uuid:uuid-to-octet-vector id)
    (simple-error () id)
    (sb-pcl::missing-slot () id)))
