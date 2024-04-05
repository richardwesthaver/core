;;; lib/doc/symbol.lisp --- Symbol Documentation

;;

;;; Code:
(in-package :doc)

(defmacro do-symbols* ((var &optional (package '*package*) result-form)
                       &body body)
  "Just like do-symbols, but makes sure a symbol is visited only once."
  (let ((seen-ht (gensym "SEEN-HT")))
    `(let ((,seen-ht (make-hash-table :test #'eq)))
       (do-symbols (,var ,package ,result-form)
         (unless (gethash ,var ,seen-ht)
           (setf (gethash ,var ,seen-ht) t)
           (tagbody ,@body))))))

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
:TRANSFORM
:VOP
:IR1-CONVERT
|#
(defun classify-symbol (symbol)
  "Returns a list of classifiers that classify SYMBOL according to its
underneath objects (e.g. :BOUNDP if SYMBOL constitutes a special
variable.) The list may contain the following classification
keywords: :BOUNDP, :FBOUNDP, :CONSTANT, :GENERIC-FUNCTION,
:TYPESPEC, :CLASS, :MACRO, :SPECIAL-OPERATOR, and/or :PACKAGE"
  (check-type symbol symbol)
  (flet ((type-specifier-p (s)
           (or (documentation s 'type)
               (not (eq (deftype-lambda-list s) :not-available)))))
    (let (result)
      (when (boundp symbol)             (push (if (constantp symbol)
                                                  :constant :boundp) result))
      (when (fboundp symbol)            (push :function result))
      (when (type-specifier-p symbol)   (push :type result))
      (when (find-class symbol nil)     (push :class result))
      (when (typep symbol 'condition) (push :condition result))
      (when (typep symbol 'structure-class) (push :structure result))
      (when (alien-type-p symbol) (push :alien-type result))
      (when (vop-p symbol) (push :vop result))
      (when (macro-function symbol)     (push :macro result))
      (when (special-operator-p symbol) (push :special-operator result))
      (when (find-package symbol)       (push :package result))
      (when (compiler-macro-function symbol) (push :compiler-macro result))
      (when (compiled-function-p symbol) (push :compiled result))
      (when (and (fboundp symbol)
                 (typep (ignore-errors (fdefinition symbol))
                        'generic-function))
        (push :generic-function result))
      result)))

(defun symbol-classification-string (symbol)
  "Return a string in the form -f-c---- where each letter stands for
boundp fboundp generic-function class macro special-operator package"
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

(defclass symbol-documentation (id) ;; package-id? (sb-c::symbol-package-id s)
  ((symbol :initarg :symbol :type symbol :accessor doc-symbol)
   (class :initarg :class :type list :accessor doc-class)
   (definitions :initform nil :initarg :definitions :type list :accessor doc-definitions)
   (specs :initform nil :initarg :specs :type list :accessor doc-specs)
   (info :initarg :info :type (or null packed-info) :accessor doc-info)
   (alloc :initarg :alloc :type list :accessor doc-alloc)))

#|
(setq *defs* 
 (loop for x across (doc-symbols (package-documentation)) collect (doc-definitions x)))

|#

(defun symbol-documentation (s)
  "Return the SYMBOL-DOCUMENTATION object of S, a symbol."
  (let ((class (classify-symbol s)))
    (multiple-value-bind (defs specs) (find-definitions s)
      (make-instance 'symbol-documentation
        :id (symbol-hash s)
        :symbol s
        :class class
        :definitions defs
        :specs specs
        :info (symbol-dbinfo s)
        :alloc (multiple-value-list (allocation-information s))))))

(defmethod print-object ((self symbol-documentation) stream)
  (with-slots (symbol class) self
    (print-unreadable-object (self stream :type t)
      (format stream "~S ~A"  symbol class))))

(defmethod doc-files ((self symbol-documentation))
   (remove-duplicates
    (remove-if
     #'null ;; definition-source-pathname is allowed to be nil,
            ;; indicating no path to definition.
    (mapcar #'definition-source-pathname (doc-definitions self)))))

(defmethod describe-object ((self symbol-documentation) stream)
  (with-slots (symbol id definitions specs alloc) self
    (print-standard-describe-header self stream)
    (describe-block (stream)
      (describe symbol stream)
      (format stream "~%Alloc Info: ~S" alloc)
      (format stream "~%Definitions: ~%")
      (loop for s in specs
            do (format stream "  ~S ~S~%" s (definition-source-pathname (pop definitions)))))))
