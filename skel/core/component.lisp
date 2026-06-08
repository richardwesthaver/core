;;; component.lisp --- Skel Component

;; 

;;; Commentary:

;;; Code:
(in-package :skel/core)

;;; Script

;; Scripts are always assumed to point to an executable file. They can be ran
;; directly with EXEC.
(defclass project-script (project-component project-meta ast)
  ((kind :initform nil :initarg :kind :accessor script-kind)))

(defmethod load-project-component ((kind (eql :script)) (form t) &key (path *default-pathname-defaults*))
  (make-instance kind :form form :path path))

(defmethod write-ast ((self project-script) stream &key (pretty t) (case :downcase) &allow-other-keys)
  (write `(,(path self)) :stream stream :pretty pretty :case case :readably t :array t :escape t))

(defun make-project-script (script)
  "Make a new PROJECT-SCRIPT."
  (apply #'make-instance 'project-script
	 (if (listp script)
	     (let ((kind (first script))
		   (path (second script)))
	       (list :path path
		     :name (pathname-name path)
		     :kind kind))
	     (list :path script
		   :name (pathname-name script)
		   :kind (when-let ((ext (pathname-type script)))
			   (keywordicate ext))))))

(defmethod exec ((self project-script))
  (sb-ext:run-program (path self) nil :output t))

(defmethod write-ast ((self project-script) stream &key)
  (with-slots (path) self
    (write-string path)))

(defmethod print-object ((self project-script) stream)
  (print-unreadable-object (self stream)
    (format stream ":~A ~A" (script-kind self) (name self))))
