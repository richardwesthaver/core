;;; lisp.lisp --- Lisp files

;; SK-LISP-FILE

;;; Code:
(in-package :skel/comp/lisp)

(defclass sk-lisp-component (sk-component component) ())
(defclass sk-lisp-file (sk-lisp-component file-component) ())

(defmethod sk-new ((self (eql :lisp)) &rest args)
  (apply #'make-instance 'sk-lisp-file args))

(defmethod sk-convert ((self file-component))
  (make-instance 'sk-lisp-file 
    :path (path self)
    :name (name self)
    :properties (component-properties self)))

(defmethod sk-compile ((self sk-lisp-file) &rest args)
  (apply 'compile-file (path self) args))

(defmethod sk-load ((self sk-lisp-file) &key (compile t))
  (if compile
      (compile-and-load (path self))
      (load (path self))))

(defmethod sk-run ((self sk-lisp-file))
  (compile-and-eval `(progn ,@(ast self))))
  
(defmethods sk-load-component 
  (((self (eql :lisp)) (form pathname) &optional (path (project-root)))
   (declare (ignore self))
   (let* ((type (pathname-type form))
          (name (namestring (if type (pathname-name form) form)))
          (fname (if type form (make-pathname :directory (namestring path) :name name :type "lisp")))
          (comp (make-instance 'sk-lisp-file :parent *skel-project* :path fname :name name)))
     comp))
  (((self (eql :lisp)) (form list) &optional (path (project-root)))
   (let ((opts (cdr form))
         (comp (sk-load-component self (pathname (car form)) (namestring path))))
     (when-let ((eval (getf opts :eval)))
       (case eval
         (:always (sk-run comp))
         ((or :never nil))
         (:load (sk-load comp :compile nil))
         ;; default is :COMPILE
         (t (sk-load comp :compile t))))
     (when (getf opts :read)
       (sk-read-file comp (path comp)))
     comp)))

(defmethod print-object ((object sk-lisp-component) stream)
  (print-unreadable-object (object stream :type t)
    (format stream ":ID ~A" (format-sxhash (id object)))))

(defmethod read-ast ((self sk-lisp-component) stream)
  (setf (ast self) (read-lisp-until-end stream)))

(defmethod sk-read-file ((self sk-lisp-component) path)
  (with-input-from-file (f path)
    (read-ast self f)))

(defmethod write-ast ((self sk-lisp-component) stream &key)
  (write (ast self) :stream stream))

(defmethod sk-write-file ((self sk-lisp-component) &key path)
  (with-output-to-file (f (or path (path self)))
    (write-ast self f)))

(defmethod load-ast ((self sk-lisp-component))
  (if (ast self)
      (prog1 (sk-run self)
        (setf (ast self) nil))
      (sk-load self)))
      
      
