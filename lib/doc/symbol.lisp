;;; lib/doc/symbol.lisp --- Symbol Documentation

;;

;;; Code:
(in-package :doc)

(deftempo :symbol-documentation
  "<%@if level%><%@repeat level%>*<%@endrepeat%><%@else%>*<%@endif%> <%@var name%><%@ifnotempty tags%> <%@var tags%><%@endif%>
:PROPERTIES:
:ID: <%@var id%>
:CUSTOM_ID: <%@var custom-id%>
:END:<%@ifnotempty documentation%>
#+begin_example
<%@var documentation%>
#+end_example
<%@endif%><%@ifnotempty alloc%>
- Alloc Info 
  - <%=(car (getf-tempo \"alloc\"))%>
    #+begin_example
    <%=(cadr (getf-tempo \"alloc\"))%>
    #+end_example
<%@endif%><%@ifnotempty set-by%>
- Set by
<%@loop set-by%>  - <%=env%>
<%@endloop%><%@endif%><%@ifnotempty bound-by%>
- Bound by
<%@loop bound-by%>  - <%=env%>
<%@endloop%><%@endif%><%@ifnotempty called-by%>
- Called by
<%@loop called-by%>  - <%=env%>
<%@endloop%><%@endif%><%@ifnotempty macroexpanded-by%>
- Macroexpanded by
<%@loop macroexpanded-by%>  - <%=env%>
<%@endloop%><%@endif%><%@ifnotempty definitions%>
:definitions:
<%@loop definitions%><%=(doc:publish env)%>
<%@endloop%>:end:
<%@endif%>")

#|
(Public)
:CLASS
:COMPILER-MACRO
:CONDITION
:CONSTANT
:FUNCTION
:GENERIC-FUNCTION
:MACRO
:METHOD
:METHOD-COMBINATION
:PACKAGE
:SETF-EXPANDER
:STRUCTURE
:SYMBOL-MACRO
:TYPE
:ALIEN-TYPE
:VARIABLE
:DECLARATION

(Internal)
:OPTIMIZER
:SOURCE-TRANSFORM
:SPECIAL-OPERATOR
:TRANSFORM
:VOP
:IR1-CONVERT

(Custom)
:SYSTEM
:MODULE
|#

(defun classify-symbol (symbol)
  "Return the classification list of SYMBOL."
  (check-type symbol symbol)
  (let (result)
    (when (boundp symbol)             (push (if (constantp symbol) :constant :boundp) result))
    (or (when (find-system symbol) (push :system result))
        (when (find-module symbol) (push :module result)))
    (when (find-class symbol nil) (push :class result))
    (when (ignore-errors (subtypep symbol 'condition)) (push :condition result))
    (when (ignore-errors (subtypep symbol 'structure-class)) (push :structure result))
    (when (ignore-errors (parse-alien-type symbol nil)) (push :alien-type result))
    (when (fboundp symbol)            (push :function result))
    (when (documentation symbol 'type) (push :type result))
    (when-let ((sym (find-symbol* symbol :sb-vm nil)))
      (when (or (gethash sym sb-c::*backend-parsed-vops*) 
                (gethash sym sb-c::*backend-template-names*))
        (push :vop result)))
    (when (macro-function symbol)     (push :macro result))
    (when (special-operator-p symbol) (push :special-operator result))
    (when (find-package symbol)       (push :package result))
    (when (compiler-macro-function symbol) (push :compiler-macro result))
    (when (compiled-function-p (ignore-errors (symbol-function symbol))) (push :compiled result))
    (when (and (fboundp symbol)
               (typep (ignore-errors (fdefinition symbol))
                      'generic-function))
      (push :generic-function result))
    result))

(defun symbol-classification-string (symbol)
  "Return a string in the form -f-c---- where each letter stands for:
- boundp 
- fboundp 
- generic-function 
- class 
- macro 
- special-operator 
- package"
  (let ((letters "bfgctmsp")
        (result (copy-seq "--------")))
    (flet ((flip (letter)
             (setf (char result (position letter letters))
                   letter)))
      (when (boundp symbol) (flip #\b))
      (when (fboundp symbol)
        (flip #\f)
        (when (typep (ignore-errors (fdefinition symbol))
                     'generic-function)
          (flip #\g)))
      (when (deftype-lambda-list symbol) (flip #\t))
      (when (find-class symbol nil)   (flip #\c) )
      (when (macro-function symbol)   (flip #\m))
      (when (special-operator-p symbol) (flip #\s))
      (when (find-package symbol)       (flip #\p))
      result)))

(defun symbol-tag-string (sym)
  "Return a string consisting of tags separated by ':'."
  (when-let ((tags (mapcar 'symbol-name (if (typep sym 'symbol-documentation)
                                            (doc-class sym)
                                            (classify-symbol sym)))))
    (with-output-to-string (s) (fmt-tags s tags))))

(defun %symbol-info (sym)
  (collecting
    (let ((prev 0))
      (sb-impl::call-with-each-info
       (lambda (name type-num val)
         (unless (eq name prev) (setq prev name))
         (let ((type (svref *info-types* type-num)))
           (collect (list name 
                          (if (not type) type-num 
                              (when type (list 
                                          (sb-impl::meta-info-category type) 
                                          (sb-impl::meta-info-kind type))))
                          val))))
       sym))))

(defun symbol-info (sym)
  (let ((ret))
    (dolist (s (%symbol-info sym) ret)
      (if-let ((l (find (car s) ret :key 'car :test 'equalp)))
        (pushnew (cdr s) (cdr l) :test 'equalp)
        (push s ret)))))

(deffmt fmt-definition-source "- ~A~@[:~A~]~@[ ~(~A~)~]")

(defmethod publish ((self definition-source) &key)
  (with-output-to-string (s)
    (fmt-definition-source 
     s
     (definition-source-pathname self) 
     (definition-source-line-number self)
     (sb-introspect::definition-source-description self))))

(defclass symbol-documentation (id) ;; package-id? (sb-c::symbol-package-id s)
  ((symbol :initarg :symbol :type symbol :accessor doc-object)
   (class :initarg :class :type list :accessor doc-class)
   (definitions :initform nil :initarg :definitions :type list :accessor doc-definitions)
   (specs :initform nil :initarg :specs :type list :accessor doc-specs)
   (info :initarg :info :type (or list packed-info) :accessor doc-info)
   (alloc :initarg :alloc :type list :accessor doc-alloc)))

(defmethod name ((self symbol-documentation))
  (symbol-name (doc-object self)))

(defmethod document-class ((self symbol-documentation)) 'org-heading)
(defmethod document-keyword ((self symbol-documentation)) :heading)

(defun symbol-documentation (s)
  "Return the documentation instance of S, a symbol."
  (let ((class (classify-symbol s))
        (defs (find-definitions s)))
    (make-instance 'symbol-documentation
      :id (symbol-hash s)
      :symbol s
      :class class
      :definitions defs
      :info (symbol-info s)
      :alloc (multiple-value-list (allocation-information s)))))

(defmethod print-object ((self symbol-documentation) stream)
  (with-slots (symbol class) self
    (print-unreadable-object (self stream :type t)
      (format stream "~S ~A"  symbol class))))

(defmethod doc-files ((self symbol-documentation))
  ;; definition-source-pathname is allowed to be nil, indicating no path to
  ;; definition.
  (flatten
   (remove-duplicates
    (mapcar #'definition-source-pathname (doc-definitions self)))))

(defmethod describe-object ((self symbol-documentation) stream)
  (with-slots (symbol id definitions alloc) self
    ;; (print-standard-describe-header self stream)
    (describe symbol stream)
    (format stream "~%Id: ~S~%" id)
    (format stream "~%Alloc Info: ~S~%" alloc)
    (format stream "~%Definitions: ~S~%" definitions)))

(deftyped normalize-source-location-alist ((l list)) list
  (mapcar
   (lambda (x) (if (listp x) (cdr x) x))
   (remove-if (lambda (x) (and (consp x) (car-eql 'lambda x)))
              (remove-duplicates 
               (mapcar 'car l)
               :test 'equalp))))

(deftyped normalize-alloc-info ((l list)) list
  (loop for (x  y) on l by 'cddr
        do (setf x (keywordicate x))
        collect x
        collect y))

(defun org-description (text)
  "Normalize the description TEXT, prepending any header-looking lines
with a comma."
  (apply 'concatenate 'string
         (loop for l in (split-sequence #\newline text)
               if (and (char= (char l 0) #\*) (whitespace-p (char l 1)))
               collect (concatenate 'string "," l)
               else collect l)))
        
(defmethod publish ((self symbol-documentation) &key output level)
  (with-slots (id definitions alloc) self
    (let ((gen (execute-template 
                (keywordicate (class-name (class-of self)))
                :env
                `(:name ,(name self) :id ,id
                  :custom-id ,(symbol-name* (doc-object self) nil)
                  :documentation ,(let ((docs (ignore-errors
                                               (trim
                                                (with-output-to-string (s)
                                                  (describe-object (doc-object self) s))))))
                                    (if (string= (name self) "*")
                                        (org-description docs)
                                        docs))
                  :tags ,(symbol-tag-string self)
                  :set-by ,(normalize-source-location-alist (who-sets (doc-object self)))
                  :bound-by ,(normalize-source-location-alist (who-binds (doc-object self)))
                  :called-by ,(normalize-source-location-alist (who-calls (doc-object self)))
                  :macroexpanded-by ,(normalize-source-location-alist (who-macroexpands (doc-object self)))
                  ,@(when level `(:level ,level))
                  :definitions 
                  ,(sort
                    (remove-duplicates
                     (remove-if (lambda (x) (not (definition-source-pathname x))) definitions)
                     :test (lambda (x y) (and (pathname-equal (definition-source-pathname x)
                                                              (definition-source-pathname y))
                                              (equalp (sb-introspect::definition-source-description x)
                                                      (sb-introspect::definition-source-description y)))))
                    (lambda (x y) (< (length (string (car (flatten (sb-introspect::definition-source-description x)))))
                                     (length (string (car (flatten (sb-introspect::definition-source-description y))))))))
                  :alloc ,(normalize-alloc-info alloc)))))
      (case output
        ('nil (values (org-parse (document-keyword self) gen) gen))
        (:string gen)
        (t (print gen output))))))
