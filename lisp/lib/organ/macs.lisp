(in-package :organ)

(defun sym-to-org-class-name (sym) 
  "Convert keyword or symbol SYM to a symbol which could designate an ORG- object type."
  (symbolicate 'org- sym))

(defmacro define-org-element (name slots &key documentation greater lesser)
  (let ((docstring (or documentation (format nil "Org ~a element class." name))))
    `(defclass ,(sym-to-org-class-name name) (,(or (when greater 'org-greater-element) 
                                                   (when lesser 'org-lesser-element) 
                                                   'org-element))
       ,slots
       (:documentation ,docstring))))

(defmacro define-org-object (name slots &key include documentation)
  (let ((docstring (or documentation (format nil "Org ~a object structure." name)))
        (obj (sym-to-org-class-name name)))
    `(defstruct (,obj ,@(when include (list `(:include ,(sym-to-org-class-name include))))) ,docstring ,@slots)))

;; (macroexpand '(define-org-parser (headline) (print headline)))
(defmacro define-org-parser ((name &key (from 'string)) &body body)
  "Define an ORG-PARSE method specializer for org type specifier NAME with body BODY."
  (once-only ((nvar (sb-int:keywordicate name)))
    `(defmethod org-parse ((type (eql ,nvar)) (input ,from))
       (let ((,name (org-create ,nvar)))
         ,@body))))

