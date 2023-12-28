;;; lib/organ/obj.lisp --- Org Heading

;;

;;; Code:
(in-package :organ)

(defun read-until-end (stream)
  (with-output-to-string (s)
    (loop for c = (read-char stream nil :eof)
          until (eql c :eof)
          do (write-char c s))))
  
(defclass org-heading () 
  ((headline :initarg :headline :initform (org-create :headline) :type org-headline)
   (planning :initarg :planning :initform nil :type (or null org-planning))
   (contents :initarg :contents :initform nil :type (or null (vector (or org-section org-heading))))))

(defmethod org-create ((type (eql :header)) &rest initargs &key &allow-other-keys)
  (apply #'make-instance (sym-to-org-class-name type) initargs))

(define-org-parser (heading :from string)
  (with-input-from-string (s input)
    (let ((headline (org-parse :headline (read-line s)))
          (contents (org-parse :section (read-until-end s))))
      (make-instance 'org-heading :headline headline :contents contents))))
