;;; obj/query/pkg.lisp --- Query Objects

;; Lisp primitive Query objects for DIY query engines.

;;; Commentary:

;; This package provides the base set of classes and methods for implementing
;; query engines.

;; The intention is to use these objects in several high-level packages where
;; we need the ability to ask complex questions about some arbitrary data
;; source.

;; The type of high-level packages can loosely be categorized as:

;; - Frontends :: The interface exposed to the user - SQL, Prolog, etc.

;; - Backends :: The interface exposed to the underlying data sources - RocksDB, SQLite, etc.

;; A 'complete' Data Management System can thus be created from combining a
;; Frontend and Backend.

;;; Code:
(in-package :obj/query)

(defvar *literal-value-types* '(boolean fixnum signed-byte unsigned-byte float double-float string))
(deftype literal-value-type () `(or ,@*literal-value-types*))
(deftype literal-value-vector () '(vector literal-value-type))

(defstruct field
  (name (symbol-name (gensym "#")) :type simple-string)
  (type t :type (or symbol list)))

(defmethod make-load-form ((self field) &optional env)
  (declare (ignore env))
  `(make-field :name ,(field-name self) :type ,(field-type self)))

(deftype field-vector () '(vector field))

(defclass schema ()
  ((fields :type (vector field) :initarg :fields :accessor fields)))

(defun make-schema (&rest fields)
  (make-instance 'schema :fields (coerce fields 'field-vector)))

(defgeneric load-schema (self &optional schema))

(defmethod make-load-form ((self schema) &optional env)
  (declare (ignore env))
  `(make-instance ,(class-of self) :fields ,(fields self)))

(defclass schema-metadata ()
  ((metadata :initarg :metadata :accessor schema-metadata)))

(defmethod make-load-form ((self schema-metadata) &optional env)
  (declare (ignore env))
  `(make-instance ,(class-of self) :metadata ,(schema-metadata self)))

(defstruct record-batch
  (schema (make-schema) :type schema)
  (fields #() :type field-vector))

(defclass data-source ()
  ((schema :type schema)))

(defmethod make-load-form ((self record-batch) &optional env)
  (declare (ignore env))
  `(make-record-batch :schema ,(record-batch-schema self) :fields ,(record-batch-fields self)))

(defgeneric field (self n)
  (:method ((self record-batch) (n fixnum))
    (svref (record-batch-fields self) n)))

(defmethod fields ((self record-batch))
  (record-batch-fields self))

(defgeneric schema (self)
  (:method ((self record-batch))
    (record-batch-schema self)))

(defgeneric derive-schema (self))

(defgeneric row-count (self)
  (:method ((self record-batch))
    (sequence:length (svref (record-batch-fields self) 0))))

(defgeneric column-count (self)
  (:method ((self record-batch))
    (length (record-batch-fields self))))

(defgeneric scan-data (self &optional projection)
  (:documentation "Scan the data source, selecting the specified columns."))

;;; Data Frame

;; minimal data-frame abstraction. methods are prefixed with 'DF-'.
(defclass data-frame ()
  ((plan :type logical-plan :accessor df-plan :initarg :plan)))

(defmethod schema ((self data-frame)) (schema (df-plan self)))

(defgeneric df-project (&rest expr &key &allow-other-keys))
(defgeneric df-filter (expr))
(defgeneric df-aggregate (group-by agg-expr))

;;; Execution Context
(defclass execution-context () ())
;;; Expression
(defclass query-expression () ())

;;;; Logical Expressions
(defclass logical-expression (query-expression) ())

(defclass column-expression (logical-expression)
  ((name :type string :initarg :name :accessor column-name)))

(defclass literal-expression (logical-expression) ())

;;;;; Alias
(defclass alias-expression (logical-expression)
  ((expr :type logical-expression :initarg :expr)
   (alias :type string :initarg :alias)))

;;;;; Unary
(defclass unary-expression (logical-expression)
  ((expr :type logical-expression)))

;;;;; Binary
(defclass binary-expression (logical-expression)
  ((lhs :type logical-expression :initarg :lhs)
   (rhs :type logical-expression :initarg :rhs)))

;;;;; Equiv Expr

;;;;; Bool Expr

;;;;; Math Expr

;;;;; Agg Expr
(deftype aggregate-function () `(function ((input logical-expression)) query-expression))

(deftype aggregate-function-designator () `(or aggregate-function symbol))

(defclass aggregate-expression (logical-expression)
  ((agg :type aggregate-function-designator)
   (expr :type logical-expression)))

;;;; Physical Expression

;; Subclasses of PHYSICAL-EXPRESSION have the suffix -EXPR
(defclass physical-expression (query-expression) ())

(defgeneric evaluate (self input)
  (:documentation "Evaluate the expression SELF with INPUT and return a result."))

(defclass column-physical-expression (physical-expression)
  ((val :type integer)))

(defclass literal-physical-expression (physical-expression) ())

(defclass binary-physical-expression (physical-expression)
  ((lhs :type physical-expression)
   (rhs :type physical-expression)))

(defclass equiv-physical-expression (binary-physical-expression) ())

(defclass math-physical-expression (binary-physical-expression) ())

(deftype accumulator (&optional (val t)) `(function () ,val))

(defgeneric accumulate (self &optional val))

(defgeneric accumulated (self))

(defclass aggregate-physical-expression (physical-expression)
  ((input :type physical-expression)
   (accumulator :type accumulator)))

(defclass max-physical-expression (aggregate-physical-expression) ())

;; max-accumulator

;; TODO


;;; Query Plan

;; Abstract superclass of schema-based query plans.
(defclass query-plan ()
  ((schema :type schema :accessor schema :initarg :schema)
   (children :type (vector query-plan))))

;;;; Logical Plan
(defclass logical-plan (query-plan)
  ((children :type (vector logical-plan) :accessor children :initarg :children)))

;;;;; Scan
(defclass scan-data (logical-plan)
  ((path :type string)
   (data-source :type data-source)
   (projection :type (vector string))))

;;;;; Projection
(defclass projection (logical-plan)
  ((input :type logical-plan)
   (expr :type (vector logical-expression))))

;;;;; Selection
(defclass selection (logical-plan)
  ((input :type logical-plan)
   (expr :type logical-expression)))

;;;;; Aggregate
(defclass aggregate (logical-plan)
  ((input :type logical-plan)
   (group-expr :type (vector logical-expression))
   (ag-expr :type (vector aggregate-expression))))


;;;; Physical Plan
(defclass physical-plan (query-plan)
  ((children :type (vector physical-plan))))

(defgeneric execute (self))

(defclass scan-exec (physical-plan)
  ((data-source :type data-source)
   (projection :type (vector string))))

(defclass projection-exec (physical-plan)
  ((input :type physical-plan)))

(defclass selection-exec (physical-plan)
  ((input :type physical-plan)))

(defgeneric filter (self columns selection))

(defclass hash-aggregate-exec (physical-plan)
  ((input :type physical-plan)
   (group-expr :type (vector physical-plan))
   (agg-expr :type (vector aggregate-physical-expression))))

;;; Planner

;; The Query Planner is effectively a compiler which translates logical
;; expressions and plans into their physical counterparts.

(defclass query-planner () ())

(defgeneric create-physical-expression (expr input)
  (:documentation "Translate logical expression EXPR and logical plan INPUT
  into a physical expression."))

(defgeneric create-physical-plan (plan)
  (:documentation "Create a physical plan from logical PLAN."))

;;; Optimizer

;; The Query Optimizer is responsible for walking a LOGICAL-PLAN and returning
;; a modified version of the same object.

;; Rule-based Optimizers: projection/predicate push-down, sub-expr elim

;; TODO: Cost-based optimizers
(defclass query-optimizer () ())

;;; Query
(defclass query () ())

(defgeneric make-query (self &rest initargs &key &allow-other-keys)
  (:method ((self t) &rest initargs)
    (declare (ignore initargs))
    (make-instance 'query)))

(defgeneric execute-query (self q))
