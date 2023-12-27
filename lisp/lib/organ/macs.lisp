(in-package :organ)

;; (sym-to-org-class-name 'headline)
(defun sym-to-org-class-name (sym) 
  "Convert keyword or symbol SYM to a symbol which could designate an ORG- object type."
  (intern (format nil "~:@(~a~a~)" "org-" sym) :organ))

(defmacro define-org-element (name slots &key documentation greater lesser)
  (let ((docstring (or documentation (format nil "Org ~a element class." name)))
        (sname (sym-to-org-class-name name)))
    `(progn
       (defclass ,sname (,(or (when greater 'org-greater-element) 
                              (when lesser 'org-lesser-element) 
                              'org-element))
         ,slots
         (:documentation ,docstring))
       (defmethod org-create ((type (eql ',(sb-int:keywordicate name))) &rest initargs)
         (apply #'make-instance (sym-to-org-class-name type) initargs))
       (export '(,sname) :organ))))

(defmacro define-org-object (name slots &key include documentation)
  (let ((docstring (or documentation (format nil "Org ~a object structure." name)))
        (obj (sym-to-org-class-name name)))
    `(progn
       (defstruct (,obj ,@(when include (list `(:include ,(sym-to-org-class-name include))))) ,docstring ,@slots)
       (export '(,obj) :organ))))

;; (macroexpand '(define-org-parser (headline) (print headline)))
(defmacro define-org-parser ((name &key (from 'string)) &body body)
  "Define an ORG-PARSE method specializer for org type specifier NAME with body BODY."
  (let ((nvar (sb-int:keywordicate name)))
    `(defmethod org-parse ((type (eql ,nvar)) (input ,from))
       (let ((,name (org-create ,nvar)))
         ,@body))))

