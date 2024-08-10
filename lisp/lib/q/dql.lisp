;;; dql.lisp --- Deductive Query Langs

;; Query Engine for Inference-based query langs.

;;; Commentary:

;; Prolog, Datalog, etc.

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

;;;;; Data Model

;; compiled code + constants -> physical plan -> arena + hash-tables -> engine

;;;;; Compiler

;; Predicates

;; Rules/Facts

;;;;; Runtime

;; Engine 

;; Execution 

;; Persistence

;;;;; Refs

;; https://franz.com/support/documentation/11.0/prolog.html

;; https://github.com/wmannis/cl-gambol

;; https://norvig.com/paip/README.html

;; https://en.wikipedia.org/wiki/Negation_as_failure

;; https://github.com/bobschrag/clolog/blob/main/architecture.md

;; https://www.swi-prolog.org/pldoc/man?section=predsummary

;; https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=cc7dcdf130adbd7be4d0ed5d3f4ea890e4477223

;;; Code:
(in-package :q/dql)

;;; Vars

(declaim (fixnum *lips*))
(defvar *lips* 0
  "Count of logical inferences performed.")

(defvar *leash-limit* nil)

(defvar *leash-indent-wrap* 20
  "The output to *LEASH-OUTPUT* indents by one level for each level of a predicate, modulo this value.")

(defvar *leash-output* (make-synonym-stream '*trace-output*))

;; from GAMBOL
(defvar *interactive* t
  "true iff interacting with user")
(defvar *auto-backtrack* nil
  "return all solutions if true")
(defvar *last-continuation* nil
  "saved state of the system")
(defvar *trail* nil
  "the trail, for backtracking")
(defvar *x-env* nil
  "env for goals")
(defvar *y-env* nil
  "env for rules")
(defvar *top-level-envs* nil
  "saves top-level environments")
(defvar *top-level-vars* nil
  "saves top-level variable names")
(defvar *rules*  (make-hash-table)
  "hash table for prolog rule heads")
(defvar *facts* nil
  "Facts are uncoditional truths. They are expressed simply as rules with no
variables in the head and no clauses in the body. During reading of a DQL
form, if we find any facts we evaluate them and store them here.")

;;; Utils
(defconstant +impossible+ 'no "make impossible look nice")
(defconstant +solved+ 'yes "make solved look nice")

(defconstant +?+ #\?)

(defun dql-variable-p (sym)
  "Valid DQL variables are symbols which start with the character #\? as in '?FOO
and '?BAR."
  (and (symbolp sym)
       (eql (char (symbol-name sym) 0) +?+)))

(deftype dql-variable () `(satisfies dql-variable-p))

(defun dql-anonymous-p (sym)
  "Return T if SYM is a DQL anonymous variable represented by the value of +?+."
  (eq sym (symbolicate +?+)))

(deftype dql-anonymous () '(satisfies dql-anonymous-p))

(defgeneric proof-tree (self))

(defgeneric print-proof-tree (self &optional stream))

;;; Conditions
(define-condition dql-error (error) ())

(deferror simple-dql-error (dql-error simple-error) ())

(defun simple-dql-error (ctrl &rest args)
  (error 'simpl-dql-error :format-control ctrl :format-arguments args))

(define-condition invalid-dql-anonymous (dql-error) ())

(define-condition invalid-dql-variable (dql-error) ())

;;; Prolog Predicates
(defun dql-predicate-p (sym)
  "Check if SYM looks like a DQL predicate. It shoulb be suffixed by a #\/
followed by either '* for vararg functors or an integer indicating the arity
of the predicate. On success returns the arity or T for varargs."
  (when-let ((arity (cdr (ssplit #\/ (symbol-name sym)))))
    (setf (the simple-string arity) (car arity))
    (or (and
         (digit-char-p (char arity 0))
         (parse-integer arity))
        (char= (char arity 0) #\*))))

;; ports: call, exit, redo, and fail

;; define-functor

;;; Lisp Operators

(defmacro <- (head &body body))
(defmacro <-- (head &body body))

(defmacro ?- (&body clauses)
  "Enter the interactive DQL execution environment, attempting to solve for
CLAUSES.")


(defmacro leash (&body (functor arity))
  "Prolog equivalent of CL:TRACE."
  (print functor) (print arity))
  
(defmacro unleash (&body (functor arity))
  "Prolog equivalent of CL:UNTRACE."
  (print functor) (print arity))

(defun prolog-compile-symbols (&rest functors))

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
(defun unify (goal))
  
;; optimistic vs pessimistic when presented with infinite recursion

;;; CLOS
(defclass dql-query (query) ())

(defclass dql-data-source (data-source) ()
  (:documentation "Data source which can be used withing DQL expressions."))

;;; Parser
(defclass dql-parser (query-parser) ())
