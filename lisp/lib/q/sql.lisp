;;; sql.lisp --- Structured Query Langs

;; SQL parser and query specification.

;;; Commentary:

;; Parser derived from PARSE/PRATT:PRATT-PARSER

;; ref: https://tdop.github.io/

;;; Code:
(in-package :q/sql)

(declaim (optimize (speed 3)))

;;; Conditions
(define-condition sql-error (error) ())

(deferror simple-sql-error (sql-error simple-error) ())

(defun simple-sql-error (ctrl &rest args)
  (error 'simple-sql-error :format-control ctrl :format-arguments args))

(define-condition sql-token-error (sql-error)
  ((token :initarg :token :reader bad-token))
  (:report (lambda (c s)
             (format s "Bad Token: ~A" (bad-token c)))))

(defun sql-token-error (token)
  (error 'sql-token-error :token token))

(define-condition illegal-sql-state (sql-error)
  ((state :initform nil :initarg :state :reader illegal-state))
  (:report (lambda (c s)
             (format s "Illegal SQL State: ~A" (illegal-state c)))))

(defun illegal-sql-state (state)
  (error 'illegal-sql-state :state state))

;;; Logical Classes
(defclass sql-query (query) ())

(defclass sql-data-source (data-source) ()
  (:documentation "Data source which can be used within SQL expressions."))

;; SQL-EXPRESSIONs are the output of a SQL-PARSER. These objects are further
;; lowered to LOGICAL-EXPRESSIONs.
(defclass sql-expression () ())

(deftype sql-expression-vector () '(vector sql-expression))

(defclass sql-identifier (id sql-expression) ())

(defclass sql-binary-expression (binary-expression sql-expression) ())

(defclass sql-math-expression (sql-binary-expression)
  ((op :initarg :op :type symbol :accessor binary-expression-op)))

(defclass sql-string (sql-expression literal-expression)
  ((value :type string :initarg :value :accessor literal-value)))

(defclass sql-number (sql-expression literal-expression)
  ((value :type number :initarg :value :accessor literal-value)))

(defclass sql-function (id sql-expression)
  ((args :type sql-expression-vector :initarg :args)))

(defclass sql-alias (sql-expression alias-expression) ())

(defclass sql-cast (sql-expression)
  ((expr :type sql-expression :initarg :expr)
   (type :type sql-identifier :initarg :type)))

(defclass sql-sort (sql-expression)
  ((expr :type sql-expression :initarg :expr)
   (asc :type boolean :initarg :asc)))

(defclass sql-relation (sql-expression) ())

(defclass sql-select (sql-relation)
  ((projection :type sql-expression-vector :initarg :projection)
   (selection :type (or sql-expression null) :initarg :selection)
   (group-by :type sql-expression-vector :initarg :group-by)
   (order-by :type sql-expression-vector :initarg :order-by)
   (having :type (or null sql-expression) :initarg :having)
   (table-name :type string :initarg :table-name)))

;;; Lexer
(eval-always
  (defvar *sql-token-types* (list :ident :str :num :kw :op :sym t))
  (deftype sql-token-type-designator () `(member ,@*sql-token-types*))
  (defvar *sql-keywords*
    (list "SCHEMA"
          "DATABASE"
          "TABLE"
          "COLUMN"
          "VIEW"
          "INDEX"
          "TRIGGER"
          "PROCEDURE"
          "TABLESPACE"
          "FUNCTION"
          "SEQUENCE"
          "CURSOR"
          "FROM"
          "TO"
          "OF"
          "IF"
          "ON"
          "FOR"
          "WHILE"
          "DO"
          "NO"
          "BY"
          "WITH"
          "WITHOUT"
          "TRUE"
          "FALSE"
          "TEMPORARY"
          "TEMP"
          "COMMENT"
          ;; create
          "CREATE"
          "REPLACE"
          "BEFORE"
          "AFTER"
          "INSTEAD"
          "EACH"
          "ROW"
          "STATEMENT"
          "EXECUTE"
          "BITMAP"
          "NOSORT"
          "REVERSE"
          "COMPILE"    
          ;; alter
          "ALTER"
          "ADD"
          "MODIFY"
          "RENAME"
          "ENABLE"
          "DISABLE"
          "VALIDATE"
          "USER"
          "IDENTIFIED"
          ;; truncate
          "TRUNCATE"
          ;; drop
          "DROP"
          "CASCADE"
          ;; insert
          "INSERT"
          "INTO"
          "VALUES"
          ;; update
          "UPDATE"
          "SET"
          ;; delete
          "DELETE"
          ;; select
          "SELECT"
          "DISTINCT"
          "AS"
          "CASE"
          "WHEN"
          "ELSE"
          "THEN"
          "END"
          "LEFT"
          "RIGHT"
          "FULL"
          "INNER"
          "OUTER"
          "CROSS"
          "JOIN"
          "USE"
          "USING"
          "NATURAL"
          "WHERE"
          "ORDER"
          "ASC"
          "DESC"
          "GROUP"
          "HAVING"
          "UNION"
          ;; others
          "DECLARE"
          "GRANT"
          "FETCH"
          "REVOKE"
          "CLOSE"
          "CAST"
          "NEW"
          "ESCAPE"
          "LOCK"
          "SOME"
          "LEAVE"
          "ITERATE"
          "REPEAT"
          "UNTIL"
          "OPEN"
          "OUT"
          "INOUT"
          "OVER"
          "ADVISE"
          "SIBLINGS"
          "LOOP"
          "EXPLAIN"
          "DEFAULT"
          "EXCEPT"
          "INTERSECT"
          "MINUS"
          "PASSWORD"
          "LOCAL"
          "GLOBAL"
          "STORAGE"
          "DATA"
          "COALESCE"
          ;; Types
          "CHAR"
          "CHARACTER"
          "VARYING"
          "VARCHAR"
          "VARCHAR2"
          "INTEGER"
          "INT"
          "SMALLINT"
          "DECIMAL"
          "DEC"
          "NUMERIC"
          "FLOAT"
          "REAL"
          "DOUBLE"
          "PRECISION"
          "DATE"
          "TIME"
          "INTERVAL"
          "BOOLEAN"
          "BLOB"
          ;; Conditionals
          "AND"
          "OR"
          "XOR"
          "IS"
          "NOT"
          "NULL"
          "IN"
          "BETWEEN"
          "LIKE"
          "ANY"
          "ALL"
          "EXISTS"
          ;; Functions
          "AVG"
          "MAX"
          "MIN"
          "SUM"
          "COUNT"
          "GREATEST"
          "LEAST"
          "ROUND"
          "TRUNC"
          "POSITION"
          "EXTRACT"
          "LENGTH"
          "CHAR_LENGTH"
          "SUBSTRING"
          "SUBSTR"
          "INSTR"
          "INITCAP"
          "UPPER"
          "LOWER"
          "TRIM"
          "LTRIM"
          "RTRIM"
          "BOTH"
          "LEADING"
          "TRAILING"
          "TRANSLATE"
          "CONVERT"
          "LPAD"
          "RPAD"
          "DECODE"
          "NVL"
          ;; Constraints
          "CONSTRAINT"
          "UNIQUE"
          "PRIMARY"
          "FOREIGN"
          "KEY"
          "CHECK"
          "REFERENCES"))

  (defvar *sql-keyword-start-chars*
    (remove-duplicates (mapcar
                        (lambda (k)
                          (declare (simple-string k))
                          (char k 0))
                        *sql-keywords*)))

  (defvar *sql-keyword-table*
    (let* ((pairs (mapcar (lambda (x) (cons (keywordicate x) x)) *sql-keywords*))
           (table (make-hash-table :size (length pairs))))
      (dolist (p pairs table)
        (setf (gethash (car p) table) (cdr p)))))

  (defvar *sql-symbol-table*
    (let* ((pairs '((:LEFT-PAREN . "(")                  
                    (:RIGHT-PAREN . ")")
                    (:LEFT-BRACE . "{")
                    (:RIGHT-BRACE . "}")
                    (:LEFT-BRACKET . "[")
                    (:RIGHT-BRACKET . "]")
                    (:SEMI . ";")
                    (:COMMA . ",")
                    (:DOT . ".")
                    (:DOUBLE-DOT . "..")
                    (:PLUS . "+")
                    (:SUB . "-")
                    (:STAR . "*")
                    (:SLASH . "/")
                    (:QUESTION . "?")
                    (:EQ . "=")
                    (:GT . ">")
                    (:LT . "<")
                    (:BANG . "!")
                    (:TILDE . "~")
                    (:CARET . "^")
                    (:PERCENT . "%")
                    (:COLON . ":")
                    (:DOUBLE-COLON . "::")
                    (:COLON-EQ . ":=")
                    (:LT-EQ . "<=")
                    (:GT-EQ . ">=")
                    (:LT-EQ-GT . "<=>")
                    (:LT-GT . "<>")
                    (:BANG-EQ . "!=")
                    (:BANG-GT . "!>")
                    (:BANG-LT . "!<")
                    (:AMP . "&")
                    (:BAR . "|")
                    (:DOUBLE-AMP . "&&")
                    (:DOUBLE-BAR . "||")
                    (:DOUBLE-LT . "<<")
                    (:DOUBLE-GT . ">>")
                    (:AT . "@")
                    (:POUND . "#")))
           (table (make-hash-table :size (length pairs))))
      (dolist (p pairs table)
        (setf (gethash (car p) table) (cdr p)))))

  (declaim (ftype (function (keyword) (values string boolean))
                  get-sql-keyword
                  get-sql-symbol))
  (defun get-sql-keyword (kw) (gethash kw *sql-keyword-table*))
  (defun get-sql-symbol (kw) (gethash kw *sql-symbol-table*)))

(defvar *sql-symbols* (hash-table-values *sql-symbol-table*))

(defvar *sql-symbol-start-chars* (remove-duplicates
                                  (mapcar (lambda (x)
                                            (declare (simple-string x))
                                            (char x 0))
                                          *sql-symbols*)))

(defstruct sql-token
  (text "" :type string)
  (type t :type sql-token-type-designator)
  (end 0 :type fixnum))

(defun num-start-p (c) (or (digit-char-p c) (char= #\. c) (char= #\- c)))
(defun ident-start-p (c) (alpha-char-p c))
(defun ident-part-p (c) (or (alpha-char-p c) (digit-char-p c) (char= #\_ c)))
(defun str-start-p (c) (or (char= #\' c) (char= #\" c)))
(defun kw-start-p (c) (member c *sql-keyword-start-chars* :test 'char=))
(defun sym-start-p (c) (member c *sql-symbol-start-chars* :test 'char=))

;; low-level token readers
(defmacro def-sql-reader (name (&rest args) &body body)
  `(defun ,(symbolicate 'read-sql- name) (,@args)
     (declare (optimize (safety 0)))
     ,@body))

(defun peek-sql-char (expected stream &optional skip-ws)
  (char= (peek-char skip-ws stream) expected))

(def-sql-reader char (stream expected &optional skip-ws)
  (when (peek-sql-char expected stream skip-ws)
    (read-char stream nil nil)))

(def-sql-reader num-token (stream)
  (make-sql-token
   :text
   (with-output-to-string (s)
     (when (read-sql-char stream #\- nil)
       (write-char #\-  s))
     (loop for x = (peek-char nil stream nil nil)
           while x
           while (or (digit-char-p x) (char= #\. x))
           do (write-char (read-char stream nil nil) s)
           finally (return s)))
   :type :num
   :end (file-position stream)))

(def-sql-reader str-token (stream)
  (let ((tok (make-sql-token :type :str))
        (terminator #\"))
    (unless (read-sql-char stream terminator)
      (setf terminator #\')
      (unless (read-sql-char stream terminator)
        (sql-token-error tok)))
    (setf (sql-token-text tok)
          (with-output-to-string (s)
            (loop for x = (peek-char nil stream) ;; must not be EOF before terminator
                  if (not (char= terminator x))
                  do (write-char (read-char stream) s)
                  else if (char= terminator x)
                  do (return (read-char stream)))))
    (setf (sql-token-end tok) (file-position stream))
    tok))

(def-sql-reader sym-token (stream)
  (let ((tok (make-sql-token :type :sym)))
    (setf (sql-token-text tok)
          (with-output-to-string (s)
            (write-char (read-char stream nil nil) s))
          (sql-token-end tok) (file-position stream))
    tok))

(defun ambiguous-ident-p (tok)
  (let ((text (sql-token-text tok)))
    (or (string-equal #.(get-sql-keyword :ORDER) text)
        (string-equal #.(get-sql-keyword :GROUP) text))))

(defun proc-ambiguous-ident (stream start)
  (declare (stream stream) (fixnum start))
  (if (equalp
       (read-sequence (make-string 2) stream :start start :end (the fixnum (+ start 2)))
       #.(get-sql-keyword :BY))
      :kw
      :ident))

(def-sql-reader ident-token (stream)
  (let ((tok (make-sql-token)))
    (if (read-sql-char stream #\`)
        (setf (sql-token-text tok)
              (with-output-to-string (s)
                (loop for x = (peek-char nil stream) ;; must not be EOF before terminator
                      if (not (char= #\` x))
                      do (write-char (read-char stream) s)
                      else do (return (read-char stream))))
              (sql-token-type tok) :ident)
        ;; may not actually be ident - we check for kw after we have a known end position
        (setf (sql-token-text tok)
              (with-output-to-string (s)
                (loop for x = (peek-char nil stream nil nil)
                      while (and x (ident-part-p x))
                      do (write-char (read-char stream) s)))))
    (setf (sql-token-end tok) (file-position stream))
    ;; resolve sql-token-type
    (cond
      ((ambiguous-ident-p tok)
       (setf (sql-token-type tok) (proc-ambiguous-ident stream (sql-token-end tok))))
      ((and (not (eql (sql-token-type tok) :ident)) (member (sql-token-text tok) *sql-keywords* :test 'string-equal))
       (setf (sql-token-type tok) :kw)))
    tok))
                      
(defun next-sql-token (stream)
  "Parse the next sql token from input STREAM else return nil."
  (block :next
    (let ((tok)
          (next (peek-char t stream nil nil)))
      (unless next
        (return-from :next tok))
      (cond
        ((num-start-p next) (read-sql-num-token stream))
        ((ident-start-p next) (read-sql-ident-token stream))
        ((str-start-p next) (read-sql-str-token stream))
        ((sym-start-p next) (read-sql-sym-token stream))
        (t (make-sql-token :end (file-position stream)))))))

(defun read-sql-stream (stream)
  (loop for tok = (next-sql-token stream)
        while tok
        collect tok))

(defun read-sql-string (sql)
  "Convert SQL string into a list of tokens. Tokens are of the form
(SQL-TYPE . VALUE)."
  (with-input-from-string (sql sql)
    (read-sql-stream sql)))

;;; Parser

;; At this point we have a sequence (list) of tokens
(defclass sql-parser (pratt-parser query-parser)
  ((tokens :type list :initarg :tokens :accessor sql-tokens)))

(defmethod next-precedence ((self sql-parser))
  (let ((token (car (sql-tokens self))))
    (if (null token)
        0
        (case (sql-token-type token)
          (:kw (string-case ((sql-token-text token) :default 0)
                 ("AS" 10)
                 ("ASC" 10)
                 ("DESC" 10)
                 ("OR" 20)
                 ("AND" 30)))
          (:sym (string-case ((sql-token-text token) :default 0)
                  (#.(get-sql-symbol :LT) 40)
                  (#.(get-sql-symbol :LT-EQ) 40)
                  (#.(get-sql-symbol :EQ) 40)
                  (#.(get-sql-symbol :BANG-EQ) 40)
                  (#.(get-sql-symbol :GT-EQ) 40)
                  (#.(get-sql-symbol :GT) 40)
                  (#.(get-sql-symbol :PLUS) 50)
                  (#.(get-sql-symbol :SUB) 50)
                  (#.(get-sql-symbol :STAR) 60)
                  (#.(get-sql-symbol :SLASH) 60)
                  (#.(get-sql-symbol :LEFT-PAREN) 70)))
          (t 0)))))

(defmethod parse-prefix ((self sql-parser))
  (let ((token (pop (sql-tokens self))))
    (unless (null token)
      (case (sql-token-type token)
        (:kw (string-case ((sql-token-text token))
               ("SELECT" (parse-select self))
               ("CAST" (parse-cast self))
               ("MAX" (make-instance 'sql-identifier :id "MAX"))
               ("INT" (make-instance 'sql-identifier :id "INT"))
               ("DOUBLE" (make-instance 'sql-identifier :id "DOUBLE"))))
        (:ident (make-instance 'sql-identifier :id (sql-token-text token)))
        (:str (make-instance 'sql-string :value (sql-token-text token)))
        (:num (make-instance 'sql-number :value (parse-number (sql-token-text token))))
        ;; unknown identifier
        (t (make-instance 'sql-identifier :id (sql-token-text token)))))))

(defmethod parse-infix ((self sql-parser) (left sql-expression) precedence)
  (let* ((tokens (sql-tokens self))
         (token (pop tokens)))
    (unless (null token)
      (case (sql-token-type token)
        (:sym (cond
                ((member (sql-token-text token) (list #.(get-sql-symbol :PLUS) #.(get-sql-symbol :SUB)
                                                      #.(get-sql-symbol :STAR) #.(get-sql-symbol :SLASH)
                                                      #.(get-sql-symbol :EQ) #.(get-sql-symbol :GT)
                                                      #.(get-sql-symbol :LT))
                         :test 'string=)
                 (pop (sql-tokens self)) ;; consume
                 (make-instance 'sql-math-expression
                   :lhs left
                   :op (sql-token-text token)
                   :rhs (parse self precedence)))
                ((string-equal "(" (sql-token-text token))
                 (pop tokens)
                 (let ((args (parse-expression-list self)))
                   (assert (string-equal (sql-token-text (pop tokens)) ")"))
                   (make-instance 'sql-function :id (id left) :args args)))
                (t nil)))
        (:kw (string-case ((sql-token-text token))
               ("AS" (pop tokens)
                     (make-instance 'sql-alias
                       :expr left
                       :alias (parse-identifier self)))
               ("AND" (pop tokens)
                      (make-instance 'sql-binary-expression
                        :lhs left
                        :op "AND"
                        :rhs (parse self precedence)))
               ("OR" (pop tokens)
                     (make-instance 'sql-binary-expression
                       :lhs left
                       :op "OR"
                       :rhs (parse self precedence)))
               ("ASC" (pop tokens))
               ("DESC" (pop tokens))))))))

(defmethod parse-order ((self sql-parser))
  (let ((sort-list)
        (sort (parse-expression self)))
    (loop while sort
          do (progn
               (case (sql-token-type sort)
                 (:ident (setf sort (make-instance 'sql-sort :expr sort :asc t)))
                 (t nil))
               (push sort sort-list)
               (let ((next (car (sql-tokens self))))
                 (when (and (eql (sql-token-type next) :sym) (string-equal (sql-token-text next) ","))
                   (pop (sql-tokens self)))
                 (setf sort (parse-expression self))))
          finally (return sort-list))))

(defmethod parse-cast ((self sql-parser))
  (let ((tokens (sql-tokens self)))
    (assert (string-equal (sql-token-text (pop tokens)) "("))
    (let* ((expr (parse-expression self))
           (alias (make-instance 'sql-alias :expr expr)))
      (assert (string-equal (sql-token-text (pop tokens)) ")"))
      (make-instance 'sql-cast :expr expr :type (slot-value alias 'alias)))))

(defmethod parse-select ((self sql-parser))
  (let ((projection (parse-expression-list self))
        table filter-expr group-by having-expr order-by 
        (tok (pop (sql-tokens self))))
    (case (sql-token-type tok)
      (:kw (string-case ((sql-token-text tok))
             ("FROM"
              (setf table (parse-expression self))
              ;; TODO 2024-06-29: 
              ;; parse optional WHERE
              (let ((next (car (sql-tokens self))))
                (when next
                  (when (string-equal "WHERE" (sql-token-text next))
                    (setf filter-expr (parse-expression self)))
                  (when (and
                         (string-equal "GROUP" (sql-token-text next))
                         (string-equal "BY" (sql-token-text (cadr (sql-tokens self)))))
                    (setf group-by (parse-expression-list self)))
                  (when (string-equal "HAVING" (sql-token-text next))
                    (setf having-expr (parse-expression self)))
                  (when (and (string-equal "ORDER" (sql-token-text next))
                             (string-equal "BY" (sql-token-text next)))
                    (setf order-by (parse-order self))))))))
      (t (illegal-sql-state tok)))
    (make-instance 'sql-select
      :projection projection
      :selection filter-expr
      :group-by group-by
      :order-by order-by
      :having having-expr
      :table-name (id table))))

(defmethod parse-expression-list ((self sql-parser))
  (log:trace! "> parse-expression-list")
  (let ((ret))
    (loop for expr = (parse-expression self)
          while expr
          do (push expr ret)
          if ;; check for comma and repeat, else return
             (let ((peek (car (sql-tokens self))))
               (and
                (eql :sym (sql-token-type peek))
                (string-equal (sql-token-text peek) #.(get-sql-symbol :comma))))
          do (pop (sql-tokens self))
          else return ret
          finally (return ret))))

(defmethod parse-expression ((self sql-parser))
  (parse self 0))

(defmethod parse-identifier ((self sql-parser))
  (let ((expr (parse-expression self)))
    (if (typep expr 'sql-identifier)
        expr
        (simple-sql-error "Expected identifier, got ~A" expr))))

(defmacro with-sql-parser ((sym &optional tokens) &body body)
  `(let ((,sym (make-instance 'sql-parser :tokens ,tokens)))
     ,@body))

(defmacro with-sql-string ((sym str) &body body)
  `(with-sql-parser (,sym (read-sql-string ,str))
     ,@body))

(defmacro with-sql-stream ((sym stream) &body body)
  `(with-sql-parser (,sym (read-sql-stream ,stream))
     ,@body))

;;; Planner
(defun make-sql-logical-expression (expr input)
  (etypecase expr
    (sql-identifier (make-instance 'column-expression :name (id expr)))
    (sql-string (literal-value expr))
    (sql-number (literal-value expr))
    ;; TODO 2024-08-04: sql-unary-expression
    (sql-binary-expression
     (let ((l (make-sql-logical-expression (lhs expr) input))
           (r (make-sql-logical-expression (rhs expr) input)))
       (etypecase expr
         (sql-math-expression
          (string-case ((binary-expression-op expr))
            ;; equiv ops
            ("=" (make-instance 'eq-expression :lhs l :rhs r))
            ("!=" (make-instance 'neq-expression :lhs l :rhs r))
            (">" (make-instance 'gt-expression :lhs l :rhs r))
            (">=" (make-instance 'gteq-expression :lhs l :rhs r))
            ("<" (make-instance 'lt-expression :lhs l :rhs r))
            ("<=" (make-instance 'lteq-expression :lhs l :rhs r))
            ;; boolean ops
            ("AND" (make-instance 'and-expression :lhs l :rhs r))
            ("OR" (make-instance 'or-expression :lhs l :rhs r))
            ;; math ops
            ("+" (make-instance 'add-expression :lhs l :rhs r))
            ("-" (make-instance 'sub-expression :lhs l :rhs r))
            ("*" (make-instance 'mult-expression :lhs l :rhs r))
            ("/" (make-instance 'div-expression :lhs l :rhs r))
            ("%" (make-instance 'mod-expression :lhs l :rhs r)))))))
    (sql-alias (make-instance 'alias-expression
                 :expr (make-sql-logical-expression (slot-value expr 'expr) input)
                 :alias (id (slot-value expr 'alias))))
    ;; TODO 2024-08-04: requires cast-expression impl in obj/query
    ;; (sql-cast (make-instance 'cast))
    (sql-function
     (when (id expr)
       (string-case ((id expr))
         ("MIN" (make-instance 'min-expression
                  :expr (make-sql-logical-expression (car (slot-value expr 'args)) input)))
         ("MAX" (make-instance 'max-expression
                  :expr (make-sql-logical-expression (car (slot-value expr 'args)) input)))
         ("SUM" (make-instance 'sum-expression
                  :expr (make-sql-logical-expression (car (slot-value expr 'args)) input)))
         ("AVG" (make-instance 'avg-expression
                  :expr (make-sql-logical-expression (car (slot-value expr 'args)) input))))))))
         
(labels ((visit (expr accum)
           (when expr
             (typecase expr
               (column-expression (accumulate accum (column-name expr)))
               (alias-expression (visit (slot-value expr 'expr) accum))
               (binary-expression
                (visit (lhs expr) accum)
                (visit (rhs expr) accum))
               (aggregate-expression (visit (slot-value expr 'expr) accum))))))
  (defun get-ref-columns (exprs)
    (let ((accum))
      (loop for expr across exprs
            collect (visit expr accum))))
  (defun get-selection-ref-columns (select table)
    (let ((accum))
      (when (slot-value select 'selection)
        (let ((filter-expr (make-sql-logical-expression (slot-value select 'selection) table)))
          (visit filter-expr accum)
          (let ((valid-cols (map 'list (lambda (x) (field-name x)) (fields (schema table)))))
            (remove-if (lambda (x) (not (member x valid-cols :test 'string-equal))) accum)))))))

(defun plan-non-aggregate-query (select df projection-expr column-names-in-selection column-names-in-projection)
  (let ((plan df))
    (unless (slot-value select 'selection)
      (return-from plan-non-aggregate-query (df-project plan projection-expr)))
    (let ((missing (member-if-not
                    (lambda (x) (member x column-names-in-projection :test 'string-equal))
                    column-names-in-selection)))
      (if (null missing)
          (setq plan (df-filter 
                      plan
                      (make-sql-logical-expression
                       (slot-value select 'selection)
                       (setf plan (df-project plan projection-expr)))))
          (let ((n (length projection-expr)))
            (setq plan (df-filter plan
                                  (make-sql-logical-expression
                                   (slot-value select 'selection)
                                   (setf plan
                                         (df-project plan
                                                     (merge 'vector
                                                            projection-expr
                                                            (mapcar
                                                             (lambda (x) (make-instance 'column-expression :name x))
                                                             missing)
                                                            (lambda (x y) (declare (ignore y)) x)))))))
            
            (df-project plan
                        (coerce
                         (loop for i below n
                               collect (make-instance 'column-expression
                                         :name (field-name (field (schema plan) i))))
                         'vector))))
      plan)))

(defun plan-aggregate-query (projection-expr select column-names-in-selection df aggregate-expr)
  (let ((plan df)
        (proj-no-agg (remove-if 'aggregate-expression-p projection-expr)))
    (when (slot-value select 'selection)
      (let* ((cols-in-proj-no-agg (get-ref-columns proj-no-agg))
            (missing (member-if-not
                      (lambda (x) (member x cols-in-proj-no-agg :test 'string-equal))
                      column-names-in-selection)))
        (if (null missing)
            (setq plan (df-filter 
                        plan
                        (make-sql-logical-expression
                         (slot-value select 'selection)
                         (setf plan (df-project plan proj-no-agg)))))
            (setq plan (df-filter
                        plan
                        (make-sql-logical-expression
                         (slot-value select 'selection)
                         (setf plan
                               (df-project plan
                                           (merge 'vector
                                                  proj-no-agg
                                                  (mapcar (lambda (x) (make-instance 'column-expression :name x))
                                                          missing)
                                                  (lambda (x y) (declare (ignore y)) x))))))))
        (df-aggregate plan
                      (map 'vector (lambda (x) (make-sql-logical-expression x plan))
                           (slot-value select 'group-by))
                      aggregate-expr)))))

(defun make-sql-df (select tables)
  "Process the given SELECT statement with the provided hash-table of
string:data-frame. Returns a data-frame."
  (let* ((table (or
                 (gethash (slot-value select 'table-name)
                          tables
                          )
                 (simple-sql-error "No table named ~A" (slot-value select 'table-name))))
         (proj (map 'vector
                    (lambda (x) (make-sql-logical-expression x table))
                    (slot-value select 'projection)))
         (cols-in-proj (get-ref-columns proj))
         (agg-count (count-if 'aggregate-expression-p proj)))
    (when (and (zerop agg-count) (not (sequence:emptyp (slot-value select 'group-by))))
      (simple-sql-error "GROUP BY without aggregate expression is not supported"))
    (let ((cols-in-sel (get-selection-ref-columns select table))
          (plan table))
      (if (zerop agg-count)
          (plan-non-aggregate-query select plan proj cols-in-sel cols-in-proj)
          (let ((pro)
                (agg)
                (n-group-cols 0)
                (group-count 0))
            (declare (fixnum n-group-cols group-count))
            (loop for expr across proj
                  do (typecase expr
                       (aggregate-expression
                        (progn
                          (push (+ n-group-cols (length agg)) pro)
                          (push expr agg)))
                       (alias-expression
                        (progn
                          (push (make-instance 'alias-expression
                                  :name (+ n-group-cols (length agg))
                                  :expr (slot-value expr 'alias))
                                pro)
                          ;; TODO 2024-08-07: does this need to be cast to aggregate-expression?
                          (push (expr expr) agg)))
                       (t (progn
                            (push group-count pro)
                            (incf group-count)))))
            (let ((plan
                    (df-project
                     (plan-aggregate-query proj select cols-in-sel plan agg)
                     pro)))
              (if-let ((having (slot-value select 'having)))
                (df-filter plan (make-sql-logical-expression having plan))
                plan)))))))

(defmethod make-df ((self sql-select) &key tables &allow-other-keys)
  (when tables
    (make-sql-df self tables)))

;;; Optimizer
(defclass sql-optimizer (query-optimizer) ())

;;; Engine
(defclass sql-engine (query-engine) ()
  (:default-initargs
   :parser (make-instance 'sql-parser)))
  
;;; Top-level Macros
(defmacro with-sql ((sym input &key (parse t) optimize execute) &body body)
  `(with-sql-parser (,sym ,@(etypecase input
                              (stream `((read-sql-stream ,input)))
                              (string `((read-sql-string ,input)))))
     ,@(cond
         (optimize `((setq ,sym (optimize (parse ,sym)))))
         (parse `((setq ,sym (parse ,sym)))))
     ,@(when execute
         `((execute (make-physical-plan ,sym))))
     ,@body))
