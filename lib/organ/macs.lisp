(in-package :organ)

(defmacro define-org-element (name slots &key documentation greater lesser)
  "Define a new org-element class."
  (let ((docstring (or documentation (format nil "Org ~a element class." name)))
        (sname (sym-to-org-class-name name)))
    (eval-always
      `(progn
         (defclass ,sname (,(or (when greater 'org-greater-element) 
                                (when lesser 'org-lesser-element) 
                                'org-element))
           ,slots
           (:documentation ,docstring))
         (defmethod org-create ((type (eql ,(sb-int:keywordicate name))) &rest initargs)
           (apply #'make-instance (sym-to-org-class-name type) initargs))
         (export '(,sname) :organ)))))

(defmacro define-org-object (name slots &key include documentation)
  "Define a new org-object class."
  (let ((docstring (or documentation (format nil "Org ~a object structure." name)))
        (obj (sym-to-org-class-name name)))
    `(progn
       (defstruct (,obj ,@(when include (list `(:include ,(sym-to-org-class-name include))))) ,docstring ,@slots)
       (defmethod org-create ((type (eql ,(sb-int:keywordicate name))) &rest initargs)
           (apply #'make-instance (sym-to-org-class-name type) initargs))
       (export '(,obj) :organ))))

;; (macroexpand '(define-org-parser (headline) (print headline)))
(defmacro define-org-parser ((name &key (from 'string)) &body body)
  "Define an ORG-PARSE method specializer for org type specifier NAME with body
BODY."
  (let ((elt (sb-int:keywordicate name)))
    (pushnew elt *org-parser-keywords*)
    `(progn
       (defmethod org-parse ((type (eql ,elt)) (input ,from))
         ,@body))))

;; It's super helpful to have our objects printed with their contents
;; when reasonable
(defmacro define-org-printer ())
