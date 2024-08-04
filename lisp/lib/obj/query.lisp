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

;; - Middleware :: interfaces which are used internally and exposed publicly -
;;   query planners/optimizers/ast

;; - Backends :: The interface exposed to the underlying data sources -
;;   RocksDB, SQLite, etc.

;;;; Refs

;; https://gist.github.com/twitu/221c8349887cec0a83b395e4cbb492a7

;; https://www1.columbia.edu/sec/acis/db2/db2d0/db2d0103.htm

;; https://howqueryengineswork.com/

;;; Code:
(in-package :obj/query)

;;; Types
(eval-always
  (defvar *literal-value-types* '(boolean fixnum signed-byte unsigned-byte float double-float string)))

(deftype literal-value-type () `(or ,@*literal-value-types*))

;;; Field
(defstruct field
  (name (symbol-name (gensym "#")) :type simple-string)
  (type t :type (or symbol list)))

(defmethod make-load-form ((self field) &optional env)
  (declare (ignore env))
  `(make-field :name ,(field-name self) :type ,(field-type self)))

;;; Field Vectors
(deftype field-vector () '(vector field))

;; convenience interface for FIELD-VECTOR
(defclass column-vector () ((data :type simple-vector :accessor column-data)))

(defclass literal-value-vector (column-vector)
  ((type :type literal-value-type :initarg :type :accessor column-type)
   (data :initarg :data :accessor column-data)
   (size :type fixnum :initarg :size :accessor column-size)))

(defgeneric column-literal-value (self)
  (:method ((self literal-value-vector))
    (column-data self)))

(defgeneric column-type (self)
  (:method ((self column-vector))
    (array-element-type (column-data self))))

(defgeneric column-value (self i)
  (:method ((self column-vector) (i fixnum))
    (aref (column-data self) i))
  (:method ((self literal-value-vector) (i fixnum))
    (if (or (< i 0) (>= i (column-size self)))
        (error 'simple-error :format-control "index out of bounds: ~A" :format-arguments i)
        (column-literal-value self))))

;;; Schema
(defclass schema ()
  ((fields :type field-vector :initarg :fields :accessor fields)))

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

(defgeneric column-size (self)
  (:method ((self column-vector))
    (length (column-data self))))

;;; Record Batch
(defstruct record-batch
  (schema (make-schema) :type schema)
  (fields #() :type field-vector))

(defmethod make-load-form ((self record-batch) &optional env)
  (declare (ignore env))
  `(make-record-batch :schema ,(record-batch-schema self) :fields ,(record-batch-fields self)))

;;; Proto
(defgeneric field (self n)
  (:method ((self record-batch) (n fixnum))
    (aref (record-batch-fields self) n)))

(defgeneric fields (self)
  (:method ((self record-batch))
    (record-batch-fields self)))

(defgeneric schema (self)
  (:method ((self record-batch))
    (record-batch-schema self)))

(defgeneric derive-schema (self))

(defgeneric select (self names)
  (:method ((self schema) (names list))
    (let* ((fields (fields self))
           (ret (make-array (length fields) :element-type 'field :fill-pointer 0
                                            :initial-element (make-field))))
      (make-instance 'schema
        :fields (dolist (n names ret)
                  (if-let ((found (find n fields :test 'equal :key 'field-name)))
                    (vector-push found ret)
                    (error 'invalid-argument :item n :reason "Invalid column name"))))))
  (:method ((self schema) (names vector))
    (let* ((fields (fields self))
           (ret (make-array (length fields) :element-type 'field :fill-pointer 0
                                            :initial-element (make-field))))
      (make-instance 'schema
        :fields (loop for n across names
                      do (if-let ((found (find n fields :test 'equal :key 'field-name)))
                           (vector-push found ret)
                           (error 'invalid-argument :item n :reason "Invalid column name"))
                      finally (return ret))))))

(defgeneric project (self indices)
  (:method ((self schema) (indices list))
    (make-instance 'schema
      :fields (coerce (mapcar (lambda (i) (aref (fields self) i)) indices) 'field-vector)))
  (:method ((self schema) (indices vector))
    (make-instance 'schema
      :fields (coerce
               (loop for i across indices
                     collect (aref (fields self) i))
               'field-vector))))

(defgeneric row-count (self)
  (:method ((self record-batch))
    (sequence:length (aref (record-batch-fields self) 0))))

(defgeneric column-count (self)
  (:method ((self record-batch))
    (length (record-batch-fields self))))

;;; Execution Context
(defclass execution-context () ())

(defclass data-source ()
  ((schema :type schema :accessor schema)))

(defgeneric scan-data-source (self projection)
  (:documentation "Scan the data source, selecting the specified columns."))

;;; Dataframes
;; minimal data-frame abstraction. methods are prefixed with 'DF-'.
(defclass data-frame ()
  ((fields :initform #() :initarg :fields :accessor df-fields)
   (data :initform #() :initarg :data :accessor df-data)))

(defgeneric df-col (self))

(defgeneric df-project (&rest expr &key &allow-other-keys))
(defgeneric df-filter (expr))
(defgeneric df-aggregate (group-by agg-expr))

;;; Expressions
(defclass query-expression () ())

(defclass query-plan ()
  ((schema :type schema :accessor schema :initarg :schema)
   (children :type (vector query-plan))))

(defclass logical-plan (query-plan)
  ((children :type (vector logical-plan) :accessor children :initarg :children)))

(defclass physical-plan (query-plan)
  ((children :type (vector physical-plan))))

;;; Logical Expressions
(defclass logical-expression (query-expression) ())

(defgeneric to-field (self input)
  (:method ((self string) (input logical-plan))
    (declare (ignore input))
    (make-field :name self :type 'string))
  (:method ((self number) (input logical-plan))
    (declare (ignore input))
    (make-field :name (princ-to-string self) :type 'number)))

(defclass column-expression (logical-expression)
  ((name :type string :initarg :name :accessor column-name)))

(defmethod to-field ((self column-expression) (input logical-plan))
  (or (find (column-name self) (fields (schema input)) :test 'equal :key 'field-name)
      (error 'invalid-argument :item (column-name self) :reason "Invalid column name")))

(defmethod df-col ((self string))
  (make-instance 'column-expression :name self))

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
  ((lhs :type logical-expression :initarg :lhs :accessor lhs)
   (rhs :type logical-expression :initarg :rhs :accessor rhs)))

(defgeneric binary-expression-name (self))
(defgeneric binary-expression-op (self))

(defclass boolean-binary-expression (binary-expression)
  ((name :initarg :name :type string :accessor binary-expression-name)
   (op :initarg :op :type symbol :accessor binary-expression-op)))

(defmethod to-field ((self boolean-binary-expression) (input logical-plan))
  (declare (ignore input))
  (make-field :name (binary-expression-name self) :type 'boolean))

;; Equiv Expr
(defclass eq-expression (boolean-binary-expression) ()
  (:default-initargs
   :name "eq"
   :op 'eq))

(defclass neq-expression (boolean-binary-expression) ()
  (:default-initargs
   :name "neq"
   :op 'neq))

(defclass gt-expression (boolean-binary-expression) ()
  (:default-initargs
   :name "gt"
   :op '>))

(defclass lt-expression (boolean-binary-expression) ()
  (:default-initargs
   :name "lt"
   :op '<))

(defclass gteq-expression (boolean-binary-expression) ()
  (:default-initargs
   :name "gteq"
   :op '>=))

(defclass lteq-expression (boolean-binary-expression) ()
  (:default-initargs
   :name "lteq"
   :op '<=))

;; Bool Expr
(defclass and-expression (boolean-binary-expression) ()
  (:default-initargs
   :name "and"
   :op 'and))

(defclass or-expression (boolean-binary-expression) ()
  (:default-initargs
   :name "or"
   :op 'or))

;; Math Expr
(defclass math-expression (binary-expression)
  ((name :initarg :name :type string :accessor binary-expression-name)
   (op :initarg :op :type symbol :accessor binary-expression-op)))

;; TODO 2024-08-03: ???
(defmethod to-field ((self math-expression) (input logical-plan))
  (declare (ignorable input))
  (make-field :name "mult" :type (field-type (to-field (lhs self) input))))

(defclass add-expression (math-expression) ()
  (:default-initargs
   :name "add"
   :op '+))

(defclass sub-expression (math-expression) ()
  (:default-initargs
   :name "sub"
   :op '-))

(defclass mult-expression (math-expression) ()
  (:default-initargs
   :name "mult"
   :op '*))

(defclass div-expression (math-expression) ()
  (:default-initargs
   :name "div"
   :op '/))

(defclass mod-expression (math-expression) ()
  (:default-initargs
   :name "mod"
   :op 'mod))

;;;;; Agg Expr
(deftype aggregate-function () `(function ((input logical-expression)) query-expression))

(deftype aggregate-function-designator () `(or aggregate-function symbol))

(defclass aggregate-expression (logical-expression)
  ((name :type string)
   (expr :type logical-expression)))

(defmethod to-field ((self aggregate-expression) (input logical-plan))
  (declare (ignorable input))
  (make-field :name (slot-value self 'name) :type (field-type (to-field (slot-value self 'expr) input))))

(defclass sum-expression (aggregate-expression) ()
  (:default-initargs
   :name "SUM"))

(defclass min-expression (aggregate-expression) ()
  (:default-initargs
   :name "MIN"))

(defclass max-expression (aggregate-expression) ()
  (:default-initargs
   :name "MAX"))

(defclass avg-expression (aggregate-expression) ()
  (:default-initargs
   :name "AVG"))

(defclass count-expression (aggregate-expression) ()
  (:default-initargs
   :name "COUNT"))

(defmethod to-field ((self count-expression) (input logical-plan))
  (declare (ignore input))
  (make-field :name "COUNT" :type 'number))

;;; Logical Plan

;;;;; Scan
(defclass scan-data (logical-plan)
  ((path :type string :initarg :path)
   (data-source :type data-source :initarg :data-source)
   (projection :type (vector string) :initarg :projection)))

(defmethod derive-schema ((self scan-data))
  (let ((proj (slot-value self 'projection)))
    (if (= 0 (length proj))
        (slot-value self 'schema)
        (select (slot-value self 'schema) proj))))

(defmethod schema ((self scan-data))
  (derive-schema self))

;;;;; Projection
(defclass projection (logical-plan)
  ((input :type logical-plan :initarg :input)
   (expr :type (vector logical-expression) :initarg :expr)))

(defmethod schema ((self projection))
  (schema (slot-value self 'input)))

;;;;; Selection
(defclass selection (logical-plan)
  ((input :type logical-plan :initarg :input)
   (expr :type logical-expression :initarg :expr)))

(defmethod schema ((self selection))
  (schema (slot-value self 'input)))

;;;;; Aggregate
(defclass aggregate (logical-plan)
  ((input :type logical-plan :initarg :input)
   (group-expr :type (vector logical-expression) :initarg :group-expr)
   (agg-expr :type (vector aggregate-expression) :initarg :agg-expr)))

(defmethod schema ((self aggregate))
  (let ((input (slot-value self 'input))
        (ret))
    (loop for g across (slot-value self 'group-expr)
          do (push (to-field g input) ret))
    (loop for a across (slot-value self 'agg-expr)
          do (push (to-field a input) ret))
    (make-schema :fields (coerce ret 'field-vector))))

;;; Physical Expression
(defclass physical-expression (query-expression) ())

(defclass literal-physical-expression (physical-expression) ())

(defgeneric evaluate (self input)
  (:documentation "Evaluate the expression SELF with INPUT and return a COLUMN-VECTOR result.")
  (:method ((self string) (input record-batch))
    (make-instance 'literal-value-vector
      :size (row-count input)
      :type 'string
      :data (sb-ext:string-to-octets self)))
  (:method ((self number) (input record-batch))
    (make-instance 'literal-value-vector :size (row-count input) :type 'number :data self)))

(defclass column-physical-expression (physical-expression)
  ((val :type array-index :initarg :val)))

(defmethod evaluate ((self column-physical-expression) (input record-batch))
  (field input (slot-value self 'val)))

(defclass binary-physical-expression (physical-expression)
  ((lhs :type physical-expression :accessor lhs :initarg :lhs)
   (rhs :type physical-expression :accessor rhs :initarg :rhs)))

(defgeneric evaluate2 (self lhs rhs))

(defmethod evaluate ((self binary-physical-expression) (input record-batch))
  (let ((ll (evaluate (lhs self) input))
        (rr (evaluate (rhs self) input)))
    (assert (= (length ll) (length rr)))
    (if (eql (column-type ll) (column-type rr))
        (evaluate2 self ll rr)
        (error "invalid state! lhs != rhs"))))

(defclass eq-physical-expression (binary-physical-expression) ())

(defmethod evaluate2 ((self eq-physical-expression) lhs rhs)
  (declare (ignore self))
  (equal lhs rhs))

(defclass neq-physical-expression (binary-physical-expression) ())

(defmethod evaluate2 ((self neq-physical-expression) lhs rhs)
  (declare (ignore self))
  (equal lhs rhs))

(defclass lt-physical-expression (binary-physical-expression) ())

(defclass gt-physical-expression (binary-physical-expression) ())

(defclass lteq-physical-expression (binary-physical-expression) ())

(defclass gteq-physical-expression (binary-physical-expression) ())

(defclass and-physical-expression (binary-physical-expression) ())

(defclass or-physical-expression (binary-physical-expression) ())

(defclass math-physical-expression (binary-physical-expression) ())

(defmethod evaluate2 ((self math-physical-expression) (lhs column-vector) (rhs column-vector))
  (coerce (loop for i below (column-size lhs)
                collect (evaluate2 self (column-value lhs i) (column-value rhs i)))
          'field-vector))

(defclass add-physical-expresion (math-expression) ())

(defmethod evaluate2 ((self add-physical-expresion) lhs rhs)
  (declare (ignore self))
  (+ lhs rhs))

(defclass sub-physical-expression (math-expression) ())

(defmethod evaluate2 ((self sub-physical-expression) lhs rhs)
  (declare (ignore self))
  (- lhs rhs))

(defclass mult-physical-expression (math-expression) ())

(defmethod evaluate2 ((self mult-physical-expression) lhs rhs)
  (declare (ignore self))
  (* lhs rhs))

(defclass div-physical-expression (math-expression) ())

(defmethod evaluate2 ((self div-physical-expression) lhs rhs)
  (declare (ignore self))
  (/ lhs rhs))

(defclass accumulator ()
  ((value :initarg :value :accessor accumulator-value)))

(defgeneric accumulate (self val)
  (:method ((self accumulator) val)
    (when val
      (setf (accumulator-value self) (+ val (accumulator-value self))))))

(defgeneric make-accumulator (self))

;; max-accumulator
(defclass max-accumulator (accumulator) ())

(defmethod accumulate ((self max-accumulator) (val number))
  (when (> val (accumulator-value self))
    (setf (accumulator-value self) val)))

(defclass aggregate-physical-expression (physical-expression)
  ((input :type physical-expression)))

(defclass max-physical-expression (aggregate-physical-expression) ())

(defmethod make-accumulator ((self max-physical-expression))
  (make-instance 'max-accumulator))

;;; Physical Plan
(defgeneric execute (self))

(defclass scan-exec (physical-plan)
  ((data-source :type data-source :initarg :data-source)
   (projection :type (vector string) :initarg :projection)))

(defmethod schema ((self scan-exec))
  (select (schema (slot-value self 'data-source)) (slot-value self 'projection)))

(defmethod execute ((self scan-exec))
  (scan-data-source (slot-value self 'data-source) (slot-value self 'projection)))

(defclass projection-exec (physical-plan)
  ((input :type physical-plan :initarg :input)
   (expr :type (vector physical-expression) :initarg :expr)))

(defmethod execute ((self projection-exec))
  (coerce
   (loop for batch across (fields (execute (slot-value self 'input)))
         collect (make-record-batch :schema (slot-value self 'schema)
                                    :fields (coerce
                                             (loop for e across (slot-value self 'expr)
                                                   collect (evaluate e batch))
                                             'field-vector)))
   '(vector record-batch)))
                                                 

(defclass selection-exec (physical-plan)
  ((input :type physical-plan :initarg :input)
   (expr :type physical-expression :initarg :expr)))

(defmethod schema ((self selection-exec))
  (schema (slot-value self 'input)))

(defmethod execute ((self selection-exec))
  (coerce
   (loop for batch across (execute (slot-value self 'input))
         with res = (coerce (evaluate (slot-value self 'expr) batch) 'bit-vector)
         with schema = (schema batch)
         with count = (column-count (fields (schema batch)))
         with filtered = (loop for i from 0 below count
                               collect (filter self (field batch i) res))
         collect (make-record-batch :schema schema :fields (coerce filtered 'field-vector)))
   '(vector record-batch)))

(defgeneric filter (self columns selection)
  (:method ((self selection-exec) (columns column-vector) (selection simple-bit-vector))
    (coerce
     (loop for i from 0 below (length selection)
           unless (zerop (bit selection i))
           collect (column-value columns i))
     'field-vector)))

(defclass hash-aggregate-exec (physical-plan)
  ((input :type physical-plan :initarg :input)
   (group-expr :type (vector physical-plan) :initarg :group-expr)
   (agg-expr :type (vector aggregate-physical-expression) :initarg :agg-expr)))

(defmethod execute ((self hash-aggregate-exec))
  (coerce 
   (loop for batch across (execute (slot-value self 'input))
         with map = (make-hash-table :test 'equal)
         with groupkeys = (map 'vector  (lambda (x) (evaluate x batch)) (slot-value self 'group-expr))
         with aggr-inputs = (map 'vector (lambda (x) (evaluate (slot-value x 'input) batch))
                                 (slot-value self 'agg-expr))
         do (loop for row-idx from 0 below (row-count batch)
                  with row-key = (map 'vector
                                      (lambda (x)
                                        (when-let ((val (column-value x row-idx)))
                                          (typecase val
                                            (octet-vector (sb-ext:octets-to-string val))
                                            (t val))))
                                      groupkeys)
                  with accs = (if-let ((val (gethash row-key map)))
                                val
                                (setf
                                 (gethash row-key map)
                                 (map 'vector
                                      #'make-accumulator
                                      (slot-value self 'agg-expr))))
                  ;; start accumulating
                  do (loop for i from 0 below (length accs)
                           for accum across accs
                           with val = (column-value (aref aggr-inputs i) row-idx)
                           return (accumulate accum val))
                     ;; collect results in array
                  with ret = (make-record-batch :schema (slot-value self 'schema)
                                                :fields (make-array (hash-table-size map)
                                                                    :element-type 'field
                                                                    :initial-element (make-field)))
                  do (loop for row-idx from 0 below (hash-table-size map)
                           for gkey being the hash-keys of map
                           using (hash-value accums)
                           with glen = (length (slot-value self 'group-expr))
                           do (loop for i from 0 below glen
                                    do (setf (aref (aref (fields ret) i) row-idx)
                                             (aref gkey i)))
                           do (loop for i from 0 below (length (slot-value self 'agg-expr))
                                    do (setf (aref (aref (fields ret) (+ i glen)) row-idx)
                                             (accumulator-value (aref accums i)))))
                  collect ret))
   '(vector record-batch)))

;;; Planner

;; The Query Planner is effectively a compiler which translates logical
;; expressions and plans into their physical counterparts.

(defclass query-planner () ())

(defgeneric make-physical-expression (expr input)
  (:documentation "Translate logical expression EXPR and logical plan INPUT
  into a physical expression.")
  (:method ((expr string) (input logical-plan))
    (declare (ignore input))
    expr)
  (:method ((expr number) (input logical-plan))
    (declare (ignore input))
    expr)
  (:method ((expr column-expression) (input logical-plan))
    (let ((i (position (column-name expr) (fields (schema input)) :key 'field-name :test 'equal)))
      (make-instance 'column-physical-expression :val i)))
  (:method ((expr binary-expression) (input logical-plan))
    (let ((l (make-physical-expression (lhs expr) input))
          (r (make-physical-expression (rhs expr) input)))
      (etypecase expr
        (eq-expression (make-instance 'eq-physical-expression :lhs l :rhs r))
        (neq-expression (make-instance 'neq-physical-expression :lhs l :rhs r))
        (gt-expression (make-instance 'gt-physical-expression :lhs l :rhs r))
        (gteq-expression (make-instance 'gteq-physical-expression :lhs l :rhs r))
        (lt-expression (make-instance 'lt-physical-expression :lhs l :rhs r))
        (lteq-expression (make-instance 'lteq-physical-expression :lhs l :rhs r))
        (and-expression (make-instance 'and-physical-expression :lhs l :rhs r))
        (or-expression (make-instance 'or-physical-expression :lhs l :rhs r))
        (add-expression (make-instance 'add-physical-expresion :lhs l :rhs r))
        (sub-expression (make-instance 'sub-physical-expression :lhs l :rhs r))
        (mult-expression (make-instance 'mult-physical-expression :lhs l :rhs r))
        (div-expression (make-instance 'div-physical-expression :lhs l :rhs r))))))

(defgeneric make-physical-plan (plan)
  (:documentation "Create a physical plan from logical PLAN.")
  (:method ((plan logical-plan))
    (etypecase plan
      (scan-data (make-instance 'scan-exec
                   :data-source (slot-value plan 'data-source)
                   :projection (slot-value plan 'projection)))
      (projection (make-instance 'projection-exec
                    :schema (make-instance 'schema
                              :fields
                              (map 'field-vector
                                   (lambda (x) (to-field x (slot-value plan 'input)))
                                   (slot-value plan 'expr)))
                    :input (make-physical-plan (slot-value plan 'input))
                    :expr (map 'vector (lambda (x) (make-physical-expression x (slot-value plan 'input)))
                               (slot-value plan 'expr))))
      (selection (make-instance 'selection-exec
                   :input (make-physical-plan (slot-value plan 'input))
                   :expr (make-physical-expression (slot-value plan 'expr) (slot-value plan 'input))))
      (aggregate (make-instance 'hash-aggregate-exec
                   :input (make-physical-plan (slot-value plan 'input))
                   :group-expr (make-physical-expression (slot-value plan 'group-expr) (slot-value plan 'input))
                   :agg-expr (make-physical-expression (slot-value plan 'agg-expr) (slot-value plan 'input)))))))

;;; Joins

;;  TODO 2024-08-02: 

;; inner-join

;; outer-join left-outer-join right-outer-join

;; semi-join

;; anti-join

;; cross-join

;;; Subqueries

;;  TODO 2024-08-02: 

;; subquery

;; correlated-subquery

;; SELECT id, name, (SELECT count(*) FROM orders WHERE customer_id = customer.id) AS num_orders FROM customers

;; uncorrelated-subquery

;; scalar-subquery

;; SELECT * FROM orders WHERE total > (SELECT avg(total) FROM sales WHERE customer_state = 'CA')

;; NOTE 2024-08-02: EXISTS, IN, NOT EXISTS, and NOT IN are also subqueries

;;; Optimizer

;; The Query Optimizer is responsible for walking a QUERY-PLAN and returning a
;; modified version of the same object. Usually we want to run optimization on
;; LOGICAL-PLANs but we also support specializing on PHYSICAL-PLAN.

;; Rule-based Optimizers: projection/predicate push-down, sub-expr elim

;; TBD: Cost-based optimizers
;; TODO 2024-07-10: 
(defclass query-optimizer () ())

(defstruct (query-vop (:constructor make-query-vop (info)))
  (info nil))

(defgeneric optimize-query (self plan))

;; Projection Pushdown
(defun extract-columns (expr input &optional accum)
  (etypecase expr
    (array-index (accumulate accum (field (fields (schema input)) expr)))
    (column-expression (accumulate accum (column-name expr)))
    (binary-expression
     (extract-columns (lhs expr) input accum)
     (extract-columns (rhs expr) input accum))
    (alias-expression (extract-columns (slot-value expr 'expr) input accum))
    ;; cast-expression
    (literal-expression nil)))

(defun extract-columns* (exprs input &optional accum)
  (mapcar (lambda (x) (extract-columns x input accum)) exprs))

(defclass projection-pushdown-optimizer (query-optimizer) ())

(defun %pushdown (plan &optional column-names)
  (declare (logical-plan plan))
  (etypecase plan
    (projection
     (extract-columns (slot-value plan 'expr) column-names)
     (let ((input (%pushdown (slot-value plan 'input) column-names)))
       (make-instance 'projection :input input :expr (slot-value plan 'expr))))
    (selection
     (extract-columns (slot-value plan 'expr) column-names)
     (let ((input (%pushdown (slot-value plan 'input) column-names)))
       (make-instance 'selection :input input :expr (slot-value plan 'expr))))
    (aggregate
     (extract-columns (slot-value plan 'group-expr) column-names)
     (extract-columns*
      (loop for x across (slot-value plan 'agg-expr) collect (slot-value x 'input))
      column-names)
     (let ((input (%pushdown (slot-value plan 'input) column-names)))
       (make-instance 'aggregate
         :input input
         :group-expr (slot-value plan 'group-expr)
         :agg-expr (slot-value plan 'agg-expr))))
    (scan-data (make-instance 'scan-data
                 :path (slot-value plan 'name)
                 :data-source (slot-value plan 'data-source)
                 :projection column-names)))) ;; maybe sort here?

(defmethod optimize-query ((self projection-pushdown-optimizer) (plan logical-plan))
  (%pushdown plan))

;;; Query
(defclass query () ())

(defgeneric make-query (self &rest initargs &key &allow-other-keys)
  (:method ((self t) &rest initargs)
    (declare (ignore initargs))
    (make-instance 'query)))
