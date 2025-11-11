;;; component.lisp --- Skel Component

;; 

;;; Commentary:

;; SK-COMPONENTs are similar in nature to ASDF/COMPONENT:COMPONENT objects but
;; much more lightweight. We use this class with the assumption that whatever
;; it's wrapping is contained within another SKEL object, such as in the
;; :COMPONENTS slots of SK-PROJECTs.

;; Container objects such as SK-PROJECT are NOT subclasses of SK-COMPONENT,
;; unlike in ASDF where systems are subclasses of components.

;;; Code:
(in-package :skel/core/obj)

(defclass sk-component (skel component ast)
  ((parent :initarg :parent :accessor parent)))

(defmethod print-object ((self sk-component) stream)
  (print-unreadable-object (self stream)
    (when-let ((name (or (name self) (format-sxhash (id self)))))
      (format stream "~A ~A" (sk-class-name self t) name))))

;;; Module

;; Again just like ASDF, we define an SK-MOD class which subclasses
;; SK-COMPONENT. The SK-MOD class is used for components which have
;; sub-components themselves.

(defclass sk-mod (sk-component sk-meta)
  ((components :initarg :components :accessor components)))

(defun make-sk-mod (form)
  "Make a new SK-MOD."
  (if (listp form)
      (apply #'make-instance 'sk-mod
	     (let* ((name (pop form))
		    (components 
		      (mapcar 
		       (lambda (f)
			 (sk-load-component (car f) (if (= 1 (length (cdr f))) (cadr f) (cdr f)) (directory-path name)))
		       form)))
	       `(:name ,name :components ,components)))
      (make-instance 'sk-mod :name form :components nil)))

(defmethod sk-new ((self (eql :mod)) &key form path)
  (let ((mod (make-sk-mod form)))
    (when path (setf (path mod) path))
    mod))

(defmethod sk-load-component ((kind (eql :mod)) (form t) &optional (path *default-pathname-defaults*))
  (sk-new kind :form form :path path))

(defmethod sk-compile ((self sk-mod) &key)
  (dolist (c (components self))
    (sk-compile c)))

(defmethod sk-build ((self sk-mod) &key)
  (dolist (c (components self))
    (sk-build c)))

;;; Script

;; Scripts are always assumed to point to an executable file. They can be ran
;; directly with SK-RUN.
(defclass sk-script (sk-component sk-meta ast)
  ((kind :initform nil :initarg :kind :accessor sk-kind)))

(defmethod sk-new ((self (eql :script)) &key form path)
  (let ((script (make-sk-script form)))
    (setf (path script) path)
    script))

(defmethod sk-load-component ((kind (eql :script)) (form t) &optional (path *default-pathname-defaults*))
  (sk-new kind :form form :path path))

(defmethod write-ast ((self sk-script) stream &key (pretty t) (case :downcase) &allow-other-keys)
  (write `(,(path self)) :stream stream :pretty pretty :case case :readably t :array t :escape t))

(defun make-sk-script (script)
  "Make a new SK-SCRIPT."
  (apply #'make-instance 'sk-script
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

(defmethod sk-run ((self sk-script))
  (sb-ext:run-program (path self) nil :output t))

(defmethod sk-write ((self sk-script) stream)
  (with-slots (path) self
    (write-string path)))

(defmethod print-object ((self sk-script) stream)
  (print-unreadable-object (self stream)
    (format stream ":~A ~A" (sk-kind self) (name self))))
