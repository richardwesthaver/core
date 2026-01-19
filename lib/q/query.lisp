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
(in-package :q/proto)
(declaim (optimize (debug 3)))
(defvar *query* nil)

;;; Protocol
(defgeneric select (self names)
  (:method ((self schema) (names list))
    (let* ((fields (fields self))
           (ret (make-array (length fields) :element-type 'field :fill-pointer 0
                                            :initial-element (make-field))))
      (make-instance 'schema
        :fields (dolist (n names ret)
                  (if-let ((found (find n fields :test 'equal :key 'name)))
                    (vector-push found ret)
                    (error 'invalid-argument :item n :reason "Invalid column name"))))))
  (:method ((self schema) (names vector))
    (let* ((fields (fields self))
           (ret (make-array (length fields) :element-type 'field :fill-pointer 0
                                            :initial-element (make-field))))
      (make-instance 'schema
        :fields (loop for n across names
                      do (if-let ((found (find n fields :test 'equal :key 'name)))
                           (vector-push found ret)
                           (error 'invalid-argument :item n :reason "Invalid column name"))
                      finally (return ret))))))

(defgeneric proj (self indices)
  (:method ((self schema) (indices list))
    (make-instance 'schema
      :fields (coerce (mapcar (lambda (i) (aref (fields self) i)) indices) 'field-vector)))
  (:method ((self schema) (indices vector))
    (make-instance 'schema
      :fields (coerce
               (loop for i across indices
                     collect (aref (fields self) i))
               'field-vector))))

;;; Expressions
(defclass query-expr (expr) ())

(defclass query-plan (ast plan)
  ((schema :type schema :accessor schema :initarg :schema)
   (ast :type (vector query-plan))))

(defclass logical-query-plan (query-plan) ())

(defclass physical-query-plan (query-plan) ())

;;; Logical Expressions
(defgeneric to-field (self input)
  (:method ((self string) (input logical-query-plan))
    (declare (ignore input))
    (make-field :name self :type 'string))
  (:method ((self number) (input logical-query-plan))
    (declare (ignore input))
    (make-field :name (princ-to-string self) :type 'number)))

(defclass column-expression (logical-expr query-expr)
  ((name :type string :initarg :name :accessor name)))

(defmethod to-field ((self column-expression) (input logical-query-plan))
  (or (find (name self) (fields (schema input)) :test 'equal :key 'name)
      (error 'invalid-argument :item (name self) :reason "Invalid column name")))

(defmethod df-col ((self string))
  (make-instance 'column-expression :name self))

;;; Alias
(defclass alias-expression (logical-expr)
  ((expr :type logical-expr :initarg :expr :accessor expr)
   (alias :type string :initarg :alias)))

(defclass cast-expression (logical-expr)
  ((expr :type logical-expr :initarg :expr :accessor expr)
   (data-type :type form :initarg :data-type)))

(defmethod to-field ((self cast-expression) (input logical-query-plan))
  (make-field :name (name (to-field (expr self) input)) :type (slot-value self 'data-type)))

;;; Unary
(defclass unary-expression (logical-expr unary-expr)
  ((expr :type logical-expr :accessor expr)))

;;; Binary
(defclass binary-expression (logical-expr binary-expr) ())

(defclass boolean-binary-expression (binary-expression)
  ((name :initarg :name :type string :accessor name)
   (op :initarg :op :type symbol :accessor expr-op)))

(defmethod to-field ((self boolean-binary-expression) (input logical-query-plan))
  (declare (ignore input))
  (make-field :name (name self) :type 'boolean))

;;; Equiv Expr
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

;;; Bool Expr
(defclass and-expression (boolean-binary-expression) ()
  (:default-initargs
   :name "and"
   :op 'and))

(defclass or-expression (boolean-binary-expression) ()
  (:default-initargs
   :name "or"
   :op 'or))

;;; Math Expr
(defclass math-expression (binary-expression)
  ((name :initarg :name :type string :accessor name)
   (op :initarg :op :type symbol :accessor expr-op)))

;; TODO 2024-08-03: ???
(defmethod to-field ((self math-expression) (input logical-query-plan))
  (declare (ignorable input))
  (make-field :name "*" :type (field-type (to-field (lhs self) input))))

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

;;; Agg Expr
(deftype aggregate-function () `(function ((input logical-expr)) query-expr))

(deftype aggregate-function-designator () `(or aggregate-function symbol))

(defclass aggregate-expression (logical-expr)
  ((name :type string)
   (expr :type logical-expr :accessor expr)))

(defgeneric aggregate-expression-p (self)
  (:method ((self aggregate-expression)) t)
  (:method ((self alias-expression)) (aggregate-expression-p (expr self)))
  (:method ((self t)) nil))

(defmethod to-field ((self aggregate-expression) (input logical-query-plan))
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

(defmethod to-field ((self count-expression) (input logical-query-plan))
  (declare (ignore input))
  (make-field :name "COUNT" :type 'number))

;;; Logical Plan

;;; Scan
(defclass scan-data (logical-query-plan)
  ((path :type string :initarg :path)
   (data-source :type data-source :initarg :data-source)
   (projection :type (vector string) :initarg :projection)))

(defmethod derive-schema ((self scan-data))
  (let ((proj (slot-value self 'projection)))
    (if (= 0 (length proj))
        (schema self)
        (select (slot-value self 'schema) proj))))

(defmethod schema ((self scan-data))
  (derive-schema self))

;;; Projection
(defclass projection (logical-query-plan)
  ((input :type logical-query-plan :initarg :input)
   (expr :type (vector logical-expr) :initarg :expr)))

(defmethod schema ((self projection))
  (schema (slot-value self 'input)))

;;; Selection
(defclass selection (logical-query-plan)
  ((input :type logical-query-plan :initarg :input)
   (expr :type logical-expr :initarg :expr)))

(defmethod schema ((self selection))
  (schema (slot-value self 'input)))

;;; Aggregate
(defclass aggregate (logical-query-plan)
  ((input :type logical-query-plan :initarg :input)
   (group-expr :type (vector logical-expr) :initarg :group-expr)
   (agg-expr :type (vector aggregate-expression) :initarg :agg-expr)))

(defmethod schema ((self aggregate))
  (let ((input (slot-value self 'input))
        (ret))
    (loop for g across (slot-value self 'group-expr)
          do (push (to-field g input) ret))
    (loop for a across (slot-value self 'agg-expr)
          do (push (to-field a input) ret))
    (apply 'make-simple-schema ret)))

;;; Limit
(defclass limit (logical-query-plan)
  ((input :type logical-query-plan :initarg :input)
   (limit :type integer)))

(defmethod schema ((self limit))
  (setf (slot-value self 'schema)
        (schema (slot-value self 'input))))

(defmethod ast ((self limit))
  (setf (slot-value self 'ast)
        (ast (slot-value self 'input))))

;;; Joins
(defclass join (logical-query-plan)
  ((left :accessor lhs)
   (right :accessor rhs)
   (on :accessor join-on)))

(defclass inner-join (join) ())
(defclass outer-join (join) ())
(defclass left-join (join) ())
(defclass right-join (join) ())
;; (defclass left-outer-join (join) ())
;; (defclass right-outer-join (join) ())
;; (defclass semi-join (join) ())
;; (defclass anti-join (join) ())
;; (defclass cross-join (join) ())

(defmethod schema ((self join))
  ;; TODO 2024-08-04: test better dupe impl
  (let ((dupes (mapcon #'(lambda (l) (when (eq (car l) (second l)) (list (car l))))
                       (coerce (join-on self) 'cons)))
        (schema (make-instance 'schema)))
    (setf (fields schema)
          (typecase self
            (right-join
             (let ((l (remove-if (lambda (x) (member x dupes :test 'string-equal)) (fields (schema (lhs self)))))
                   (r (fields (schema (rhs self)))))
               (merge 'vector l r (lambda (x y) (declare (ignore y)) x))))
            (inner-join
             (let ((l (fields (schema (lhs self))))
                   (r (remove-if (lambda (x) (member x dupes :test 'string-equal)) (fields (schema (rhs self))))))
               (merge 'vector l r (lambda (x y) (declare (ignore y)) x))))))
    schema))

(defmethod ast ((self join))
  (vector (lhs self) (rhs self))) 

;;; Subqueries

;;  TODO 2024-08-02: 

;; subquery

;; correlated-subquery

;; SELECT id, name, (SELECT count(*) FROM orders WHERE customer_id = customer.id) AS num_orders FROM customers

;; uncorrelated-subquery

;; scalar-subquery

;; SELECT * FROM orders WHERE total > (SELECT avg(total) FROM sales WHERE customer_state = 'CA')

;; NOTE 2024-08-02: EXISTS, IN, NOT EXISTS, and NOT IN are also subqueries

;;; Dataframes
(defgeneric df-proj (df exprs)
  (:method ((df data-frame) (expr list))
    (df-proj df (coerce expr 'vector)))
  (:method ((df data-frame) (expr vector))
    (setf (df-plan df)
          (make-instance 'projection
            :input (df-plan df)
            :expr expr))
    df))

(defgeneric df-filter (df expr)
  (:method ((df data-frame) (expr logical-expr))
    (setf (df-plan df)
          (make-instance 'selection :input (df-plan df) :expr expr))
    df))

(defgeneric df-aggregate (df group-by agg-expr)
  (:method ((df data-frame) (group-by vector) (agg-expr vector))
    (setf (df-plan df)
          (make-instance 'aggregate :input (df-plan df)
                         :group-expr group-by
                         :agg-expr agg-expr))
    df)
  (:method ((df data-frame) (group-by list) (agg-expr list))
    (df-aggregate df (coerce group-by 'vector) (coerce agg-expr 'vector))))

;;; Physical Expression
(defclass literal-physical-expression (physical-expr literal-expr) ())

(defgeneric evaluate (self input)
  (:documentation "Evaluate the expression SELF with INPUT and return a COLUMN-VECTOR result.")
  (:method ((self string) (input record-batch))
    (make-instance 'literal-value-vector
      :size (row-count input)
      :type 'string
      :data (sb-ext:string-to-octets self)))
  (:method ((self number) (input record-batch))
    (make-instance 'literal-value-vector :size (row-count input) :type 'number :data self)))

(defclass column-physical-expression (physical-expr)
  ((val :type array-index :initarg :val)))

(defmethod evaluate ((self column-physical-expression) (input record-batch))
  (field input (slot-value self 'val)))

;;; Binary
(defclass binary-physical-expression (physical-expr)
  ((lhs :type physical-expr :accessor lhs :initarg :lhs)
   (rhs :type physical-expr :accessor rhs :initarg :rhs)))

(defgeneric evaluate2 (self lhs rhs))

(defmethod evaluate ((self binary-physical-expression) (input record-batch))
  (let ((ll (evaluate (lhs self) input))
        (rr (evaluate (rhs self) input)))
    (assert (= (length ll) (length rr)))
    (if (eql (column-type ll) (column-type rr))
        (evaluate2 self ll rr)
        (error "invalid state: lhs != rhs"))))

;;; Equiv
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

;;; Math
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

;;; Aggregate
(defclass aggregate-physical-expression (physical-expr)
  ((input :type physical-expression)))

(defclass max-physical-expression (aggregate-physical-expression) ())

(defmethod make-accumulator ((self max-physical-expression))
  (make-instance 'max-accumulator))

;;; Physical Plan
(defmethod exec ((self data-frame))
  (exec (df-plan self)))

(defclass scan-exec (physical-query-plan)
  ((data-source :type data-source :initarg :data-source)
   (projection :type (vector string) :initarg :projection)))

(defmethod schema ((self scan-exec))
  (select (schema (slot-value self 'data-source)) (slot-value self 'projection)))

(defmethod exec ((self scan-exec))
  (scan-data (slot-value self 'data-source) (slot-value self 'projection)))

(defclass projection-exec (physical-query-plan)
  ((input :type physical-query-plan :initarg :input)
   (expr :type (vector physical-expr) :initarg :expr)))

(defmethod exec ((self projection-exec))
  (coerce
   (loop for batch across (fields (exec (slot-value self 'input)))
         collect (make-record-batch :schema (slot-value self 'schema)
                                    :fields (coerce
                                             (loop for e across (slot-value self 'expr)
                                                   collect (evaluate e batch))
                                             'field-vector)))
   '(vector record-batch)))

(defclass selection-exec (physical-query-plan)
  ((input :type physical-query-plan :initarg :input)
   (expr :type physical-expr :initarg :expr)))

(defmethod schema ((self selection-exec))
  (schema (slot-value self 'input)))

(defmethod exec ((self selection-exec))
  (coerce
   (loop for batch across (exec (slot-value self 'input))
         with res = (coerce (evaluate (slot-value self 'expr) batch) 'bit-vector)
         with schema = (schema batch)
         with count = (column-count (fields (schema batch)))
         with filtered = (loop for i from 0 below count
                               collect (filter self (field batch i) res))
         collect (make-record-batch :schema schema :fields (coerce filtered 'field-vector)))
   '(vector record-batch)))

(defmethod filter ((self selection-exec) (columns column-vector) (selection simple-bit-vector))
  (coerce
   (loop for i from 0 below (length selection)
         unless (zerop (bit selection i))
         collect (column-value columns i))
   'field-vector))

(defclass hash-aggregate-exec (physical-query-plan)
  ((input :type physical-query-plan :initarg :input)
   (group-expr :type (vector physical-query-plan) :initarg :group-expr)
   (agg-expr :type (vector aggregate-physical-expression) :initarg :agg-expr)))

(defmethod exec ((self hash-aggregate-exec))
  (coerce 
   (loop for batch across (exec (slot-value self 'input))
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
                                             (accumulated (aref accums i)))))
                  collect ret))
   '(vector record-batch)))

;;; Planner

;; The Query Planner is effectively a compiler which translates logical
;; expressions and plans into their physical counterparts.

(defclass query-planner (planner) ())

(defgeneric make-physical-expression (expr input)
  (:documentation "Translate logical expression EXPR and logical plan INPUT
  into a physical expression.")
  (:method ((expr string) (input logical-query-plan))
    (declare (ignore input))
    expr)
  (:method ((expr number) (input logical-query-plan))
    (declare (ignore input))
    expr)
  (:method ((expr column-expression) (input logical-query-plan))
    (let ((i (position (name expr) (fields (schema input)) :key 'name :test 'equal)))
      (make-instance 'column-physical-expression :val i)))
  (:method ((expr binary-expression) (input logical-query-plan))
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
  (:documentation "Create a physical plan from logical plan."))

;; ;; Control Stack dies here?
;; (defmethod make-physical-plan ((plan logical-query-plan))
;;   (etypecase plan
;;     (scan-data (make-instance 'scan-exec
;;                  :data-source (slot-value plan 'data-source)
;;                  :projection (slot-value plan 'projection)))
;;     (projection (make-instance 'projection-exec
;;                   :schema (make-instance 'schema
;;                             :fields
;;                             (map 'field-vector
;;                                  (lambda (x) (to-field x (slot-value plan 'input)))
;;                                  (slot-value plan 'expr)))
;;                   :input (make-physical-plan (slot-value plan 'input))
;;                   :expr (map 'vector (lambda (x) (make-physical-expression x (slot-value plan 'input)))
;;                              (slot-value plan 'expr))))
;;     (selection (make-instance 'selection-exec
;;                  :input (make-physical-plan (slot-value plan 'input))
;;                  :expr (make-physical-expression (slot-value plan 'expr) (slot-value plan 'input))))
;;     (aggregate (make-instance 'hash-aggregate-exec
;;                  :input (make-physical-plan (slot-value plan 'input))
;;                  :group-expr (make-physical-expression (slot-value plan 'group-expr) (slot-value plan 'input))
;;                  :agg-expr (make-physical-expression (slot-value plan 'agg-expr) (slot-value plan 'input))))))

;;; Optimizer

;; The Query Optimizer is responsible for walking a QUERY-PLAN and returning a
;; modified version of the same object. Usually we want to run optimization on
;; LOGICAL-QUERY-PLANs but we also support specializing on PHYSICAL-QUERY-PLAN.

;; Rule-based Optimizers: projection/predicate push-down, sub-expr elim

;; Lowerings: hdsl -> ldsl

;; Extensibility principle - A low level DSL should have greater than or equal
;; to expressiveness of a high level DSL

;; Transformation cohesion principle - There should be a unique path lowering
;; a high-level DSL to a low-level DSL. This also prevents loops between high
;; and low level DSLs.

;; TBD: Cost-based optimizers
;; TODO 2024-07-10: 
(defclass query-optimizer () ())

(defstruct (query-vop (:constructor make-query-vop (info)))
  "A virtual query operation available to query compilers."
  (info nil))

(defgeneric optimize-query (self plan)
  (:documentation "Optimize the query expressed by PLAN using the optimizer SELF."))

;; Projection Pushdown
(defun extract-columns (expr input &optional accum)
  "Recursively check an expression for field indicators and add the to an
accumulator."
  (etypecase expr
    (array-index (accumulate accum (field (fields (schema input)) expr)))
    (column-expression (accumulate accum (name expr)))
    (binary-expression
     (extract-columns (lhs expr) input accum)
     (extract-columns (rhs expr) input accum))
    (alias-expression (extract-columns (expr expr) input accum))
    (cast-expression (extract-columns (expr expr) input accum))
    (literal-expr nil)))

(defun extract-columns* (exprs input &optional accum)
  (mapcar (lambda (x) (extract-columns x input accum)) exprs))

(defclass projection-pushdown-optimizer (query-optimizer) ())

(defun %pushdown (plan &optional column-names)
  (declare (logical-query-plan plan))
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

(defmethod optimize-query ((self projection-pushdown-optimizer) (plan logical-query-plan))
  (%pushdown plan))

;;; Query
(defclass query () ()
  (:documentation "Base class of query objects."))

(defclass simple-query (query ast id) ())

(defgeneric make-query (self &rest initargs &key &allow-other-keys)
  (:documentation "Make a new QUERY object.")
  (:method ((self t) &rest initargs)
    (apply 'make-instance 'query initargs))
  (:method ((self (eql :simple)) &rest initargs &key &allow-other-keys)
    (apply 'make-instance 'simple-query initargs)))

;;; Execution Context
(defclass execution-context () ()
  (:documentation "Base class for objects which provide enough context to a QUERY-ENGINE to
EXECUTE a DATA-FRAME."))

(defgeneric register-df (self name df)
  (:documentation "Register a DATA-FRAME with an EXECUTION-CONTEXT."))

(defgeneric register-data-source (self name source)
  (:documentation "Register a DATA-SOURCE with an EXECUTION-CONTEXT."))

(defgeneric register-file (self name path &key type &allow-other-keys)
  (:documentation "Register a DATA-SOURCE contained in a file of type TYPE at PATH."))

(defgeneric execute (self df)
  (:documentation "Execute the DATA-FRAME DF given context SELF.")
  (:method ((self t) (df data-frame))
    (declare (ignorable self))
    (exec df)))

(defmethod exec ((self logical-query-plan))
  (exec
   (make-physical-plan
    (optimize-query (make-instance 'projection-pushdown-optimizer) self))))

;;; Engine                                                                 
;; (sb-mop:class-slots (find-class 'query-engine)) ;; service schema       
(defclass query-engine (query-planner execution-context data-source engine)
  ((sources :initarg :sources)                                             
   (parser :initarg :parser :type query-parser)                            
   (optimizer :initarg :optimizer :type query-optimizer)))
