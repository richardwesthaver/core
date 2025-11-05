;;; dql.lisp --- Deductive Query Language

;; Query Engine for Inference-based query langs.

;;; Commentary:

;; Prolog, Datalog, etc.

;; Prolog rules are created with:  (*- head body body ...)
;; Prolog queries are posed with:  (?- goal goal ...)

;;;; SQL vs Prolog

;; My current understanding is that Prolog and SQL-derived langs share much in
;; common. You can certainly do deductive logic in SQL and you can do
;; relational table-based logic in Prolog.

;; It is interesting to note that they were both discovered around the same
;; time in the 60s-70s, but in very different contexts. Prolog was intended
;; for NLP and SQL (relational-algebra/RA) was the foundation for
;; RDBMS. Prolog never really found the same sort of success had by SQL, but
;; with the AI summer in full bloom and NLP being a hot-topic, perhaps we will
;; see the scales shift.

;;;; Design

;; The WAM compiler is a bit too much to understand let alone implement at
;; this stage. The design of this package will be much simpler and optimized
;; for compatibility with Lisp Objects.

;; The design we're going for in this package is what I would consider the
;; Lisper's version of Datalog. We want to implement just enough to be useful
;; as a query language, and then use it to bootstrap a more elegant Prolog
;; compiler, likely in SYN/PROLOG.

;;;; Data Model

;; forms + specials -> logical plan -> physical plan -> engine

;;;; Compiler

;; As always our 'compiler' for this DSL will be encapsulated in a series of
;; macros. In this case we will be leveraging CLtL2 where possible as well as
;; some internal SB-C functions and structures.

;; In CLtL2 terminology, we make use of the environment with functions such as
;; AUGMENT-ENVIRONMENT, PARSE-MACRO, and ENCLOSE. During compilation we build
;; the environment and then lexically bind it during execution of queries.

;;; Clauses

;; Rules are made up of calls to predicates which are called the Rule's
;; Goals. Facts are clauses without a body.

;; Rules and Facts are compiled into Functors which consist of the function
;; name and arity.

;;;; Refs

;; https://franz.com/support/documentation/11.0/prolog.html

;; https://github.com/wmannis/cl-gambol

;; https://norvig.com/paip/README.html

;; https://en.wikipedia.org/wiki/Negation_as_failure

;; https://github.com/bobschrag/clolog/blob/main/architecture.md

;; https://www.swi-prolog.org/pldoc/man?section=predsummary

;; https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=cc7dcdf130adbd7be4d0ed5d3f4ea890e4477223

;; https://en.wikipedia.org/wiki/SLD_resolution

;;; Code:
(in-package :q/dql)

;;; Vars
(defvar *leash-limit* nil)

(defvar *leash-indent-wrap* 20
  "The output to *LEASH-OUTPUT* indents by one level for each level of a predicate, modulo this value.")

(defvar *leash-output* (make-synonym-stream '*trace-output*))

;; from GAMBOL
(defvar *interactive-query* t
  "non-nil iff interacting with user")
(defvar *auto-backtrack* nil
  "return all solutions if non-nil")
(defvar *last-continuation* nil
  "saved state of the system")
(defvar *trail* nil
  "the trail, for backtracking")
(defvar *goal-env* nil
  "env for goals")
(defvar *rule-env* nil
  "env for rules")
(defvar *global-envs* nil
  "top-level environments")
(defvar *global-vars* nil
  "top-level variable names")

(defvar *functors* (make-hash-table)
  "hash table for DQL functors. Keys are the functor name and values are either a
single function (indicating a '&rest' lambda-list with arity *) or a vector of
functions with length equal to the functor with the highest arity.")

(defvar *rules*  (make-hash-table)
  "hash table for rules. Keys are the HEAD and values are the body.")
(defvar *facts* nil
  "Facts are unconditional truths. They are expressed simply as rules with no
variables in the head and no clauses in the body. During reading of a DQL
form, if we find any facts we evaluate them and store them here.")

;;; Utils
(defconstant +impossible+ :never "make impossible look nice")
(defconstant +solved+ :ok "make solved look nice")
(defconstant +dql-vars-property+ :dql-vars)
(defconstant +dql-funs-property+ :dql-funs)

(defconstant +dql-variable-prefix+ #\?)

(defun dql-variable-p (sym)
  "Valid DQL variables are symbols which start with the character #\? as in '?FOO
and '?BAR."
  (and (symbolp sym)
       (eql (char (symbol-name sym) 0) +dql-variable-prefix+)))

(deftype dql-variable () '(satisfies dql-variable-p))

(defun dql-anonymous-p (sym)
  "Return T if SYM is a DQL anonymous variable represented by the value of +?+."
  (eq sym (symbolicate +dql-variable-prefix+)))

(deftype dql-anonymous () '(satisfies dql-anonymous-p))

(defgeneric proof-tree (self))

(defgeneric print-proof-tree (self &optional stream))

;; functors
(defun match-dql-variable (a b)
  (and (eq +dql-vars-property+ (car b))
       (find a (cadr b))))

(defun match-dql-function (a b)
  (and (eq +dql-funs-property+ (car b))
       (find a (cadr b))))

(defun register-dql-variable (name env)
  (push name (cdr (assoc +dql-vars-property+ (lexenv-user-data env)))))

(defun register-dql-rule (name env &optional arity)
  (push name (cdr (assoc +dql-funs-property+ (lexenv-user-data env))))
  (setf (gethash name *functors*)
        (or (when arity (make-array arity :element-type 'function)) (function name))))

(defun register-dql-functor (name fname arity)
  (let ((val (gethash name *functors*)))
    (etypecase val
      (function (simple-dql-error "Unable to overwrite a vararg functor: ~A" name))
      (vector (setf (aref val arity) fname)))))

(defun dvboundp! (var &optional env)
  "Check if VAR is bound as a DQL-VARIABLE in the given environment."
  (lexenv-find var :user-data :lexenv env :test 'match-dql-variable))

(defun dfboundp! (fun &optional env)
  "Check if FUN is bound as a DQL-FUNCTOR in the given environment."
  (lexenv-find fun :user-data :lexenv env :test 'match-dql-function))

;; (defmacro dquoty (form &environment env)
;;   "Like QUOTY but builds DQL functors instead of functions from unknown lists."
;;   )

(defun term-to-head (term)
  (etypecase term
    (atom (values (symbolicate term '/*) nil))
    (cons (values (symbolicate (car term) '/ (length #1=(cdr term))) #1#))))

(defmacro generate-rule (head &body clauses &environment env)
  "Generate a rule with a set of clauses which may or may not eventually return T."
  (multiple-value-bind (fname args) (term-to-head head)
      `(prog1 (defun ,fname ,args ,@clauses)
         (register-dql-rule ,head ,env)
         (register-dql-functor ,head ,fname ,(length args)))))

(defmacro generate-fact (head)
  "Generate a fact which is like a rule but contains no substantial body (always returns T)."
  (multiple-value-bind (fname args) (term-to-head head)
    `(prog1 (defun ,fname ,args t)
       (register-dql-functor ,head ,fname ,(length args)))))

(defmacro generate-variable (term val &environment env)
  "Bind the symbol TERM to VAL in the specified ENV."
  `(register-dql-variable ,term ,val ,env))

(defun dql-functor-p (sym)
  "Check if SYM looks like a DQL functor. It shoulb be suffixed by a #\/
followed by either '* for vararg functors or an integer indicating the arity
of the predicate. On success returns the arity or T for varargs."
  (when-let ((arity (cdr (ssplit #\/ (symbol-name sym)))))
    (setf (the simple-string arity) (car arity))
    (or (and
         (digit-char-p (char arity 0))
         (parse-integer arity))
        (char= (char arity 0) #\*))))

;;; Conditions
(define-condition dql-condition () ())
(define-condition dql-error (dql-condition error) ())

(deferror simple-dql-error (dql-error simple-error) () (:auto t))

(define-condition invalid-dql-anonymous (dql-error) ())

(define-condition invalid-dql-variable (dql-error) ())

;;; Predicates
;; (defmacro define-dql-predicate ())
;; ports: call, exit, redo, and fail

;;; Query Protocol
;; variables are basically just fields?
(defclass dql-env (simple-schema)
  ((env :initarg :env :accessor env))
  (:default-initargs
   :env (make-null-lexenv)
   :name (symbol-name (gensym "dql-env"))))

(defmacro new-dql-env (&body fields &environment env)
  `(progn
     (appendf (lexenv-user-data ,env) '((#.+dql-vars-property+) (#.+dql-funs-property+)))
     (make-instance 'dql-env :fields (make-fields ,@fields) :env ,env)))

(defclass dql-logical-plan (logical-query-plan) ())
(defclass dql-physical-plan (physical-query-plan) ())
(defclass dql-planner (query-plan) ())

(defclass dql-expr (query-expr unary-expression)
  ((name :initarg :name :type string :accessor name)))

(defclass dql-rule-expr (dql-expr) ())
(defclass dql-fact-expr (dql-expr literal-expression) ())

(defclass dql-rule (physical-expression) 
  ())
(defclass dql-fact (physical-expression) ())

(defmethod evaluate ((self dql-rule) (input record-batch)))
(defmethod evaluate ((self dql-fact) (input record-batch)))

(defmethod make-physical-expression ((expr dql-expr) (input dql-logical-plan)))

(defmethod make-physical-plan ((plan dql-logical-plan)))

(defclass unify (dql-logical-plan) ())
(defclass solve (dql-logical-plan) ())

(defclass unify-exec (dql-physical-plan) ())
(defclass solve-exec (dql-physical-plan) ())

;; cut
;; ref: https://en.wikipedia.org/wiki/Cut_(logic_programming)
(defconstant +!+ #\!)
(defun ! () )
(define-symbol-macro ! (!))

;; equality

;; db manipulation

;; assert, retract, asserta, and assertz

;;; Resolution

;; herbrand universe | ground-terms + pred(P) -> herbrand base | map(true) -> herbrand interpretation

;;;; Unification
;;(defun unify (goal))
  
;; optimistic vs pessimistic when presented with infinite recursion

;;; Query
(defclass dql-query (query) ())

(defclass dql-data-source (data-source) ()
  (:documentation "Data source which can be used with DQL expressions."))

;;; Parser
;; (defclass dql-parser (query-parser) ())

;;; Lisp Interface
;; (defmacro *- (head &body body))
;; bindings?
;; (defmacro <- (head &body body))
;; (defmacro <-- (head &body body))

(defmacro ?- (&body goals)
  "Enter the interactive DQL execution context, attempting to solve for
GOALS."
  `(let ((*interactive-query* t)
         (*auto-backtrack* nil))
     (dql-solve ,goals)))

;; (defmacro leash (&body (functor arity))
;;   "Prolog equivalent of CL:TRACE."
;;   (print functor) (print arity))

;; (defmacro unleash (&body (functor arity))
;;   "Prolog equivalent of CL:UNTRACE."
;;   (print functor) (print arity))

;; (defun compile-dql-symbols (&rest functors))
