;;; lib/doc/symbol.lisp --- Symbol Documentation

;;

;;; Code:
(in-package :doc)

(deftempo :symbol-documentation
  "* <%@var name%><%@ifnotempty tags%> <%@var tags%><%@endif%>
:PROPERTIES:
:ID: <%@var id%>
:CUSTOM_ID: <%@var name%>
:END:
<%@var documentation%>
<%@ifnotempty definitions%>
<%@loop definitions%>
<%=(doc:publish env)%>
<%@endloop%>
<%@endif%>
<%@ifnotempty specs%>
<%@loop specs%>
<%=env%>
<%@endloop%>
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

(defmethod publish ((self definition-source) &key)
  (format nil "- ~A~@[~%  Line ~A~]~@[~%  ~A~]" 
          (definition-source-pathname self) (definition-source-line-number self)
          (sb-introspect::definition-source-description self)))

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
  (let ((class (classify-symbol s)))
    (multiple-value-bind (specs defs) (find-definitions s)
      (make-instance 'symbol-documentation
        :id (make-v5-uuid +namespace-oid+ (symbol-name* s))
        :symbol s
        :class class
        :definitions defs
        :specs specs
        :info (symbol-info s)
        :alloc (multiple-value-list (allocation-information s))))))

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
  (with-slots (symbol id definitions specs alloc) self
    ;; (print-standard-describe-header self stream)
    (describe symbol stream)
    (format stream "~%Id: ~S~%" id)
    (format stream "~%Alloc Info: ~S~%" alloc)
    (format stream "~%Definitions: ~S~%" definitions)
    (format stream "~%Specs: ~%")
    (loop for s in specs
          do (format stream "  ~S ~S~%" (definition-source-pathname s)
                     (sb-introspect::definition-source-description s)))))

(defmethod publish ((self symbol-documentation) &key output)
  (with-slots (id definitions specs alloc) self
    (let ((gen (execute-template (keywordicate (class-name (class-of self))) 
                                 :env
                                 `(:name ,(name self) :id ,id
                                   :documentation ,(with-output-to-string (s) (describe-object (doc-object self) s))
                                   :tags ,(symbol-tag-string self)
                                   :definitions ,definitions
                                   :alloc ,alloc
                                   :specs ,specs))))
      (case output
        ('nil (values (org-parse (document-keyword self) gen) gen))
        (:string gen)
        (t (print gen output))))))
