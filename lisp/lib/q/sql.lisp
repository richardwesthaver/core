;;; sql.lisp --- Structured Query Langs

;; SQL parser and query specification.

;;; Commentary:

;; Pratty TDOP-based parser: https://tdop.github.io/

;;; Code:
(in-package :q/sql)

(define-condition sql-error (error) ())

(deferror simple-sql-error (sql-error simple-error) () (:auto t))

(deferror illegal-sql-state (sql-error) ((state :initarg :state)))

(defclass sql-query (query) ())

(defclass sql-data-source (data-source) ()
  (:documentation "Data source which can be used within SQL expressions."))

(defclass sql-expression () ())

(deftype sql-expression-vector () '(vector sql-expression))

(defclass sql-identifier (id sql-expression) ())

(defclass sql-binary-expr (binary-expression sql-expression) ())

(defclass sql-string (sql-expression literal-expression)
  ((value :type string :initarg :value)))

(defclass sql-number (sql-expression literal-expression)
  ((value :type number :initarg :value)))

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

(defclass sql-planner (query-planner) ())

(defclass sql-optimizer (query-optimizer) ())

;;; Lexer
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
  (remove-duplicates (mapcar (lambda (k) (aref k 0)) *sql-keywords*)))

(defvar *sql-keyword-table*
  (let* ((pairs (mapcar (lambda (x) (cons (keywordicate x) x)) *sql-keywords*))
         (table (make-hash-table :size (length pairs))))
    (dolist (p pairs table)
      (setf (gethash (car p) table) (cdr p)))))

(defun get-sql-keyword (kw) (gethash kw *sql-keyword-table*))

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

(eval-always
  (defun get-sql-symbol (kw) (gethash kw *sql-symbol-table*)))

(defvar *sql-symbols* (hash-table-values *sql-symbol-table*))

(defvar *sql-symbol-start-chars* (remove-duplicates (mapcar (lambda (x) (aref x 0)) *sql-symbols*)))

(defstruct sql-token
  (text "" :type string)
  (type t :type sql-token-type-designator)
  (end 0 :type fixnum))

(defun num-start-p (c) (or (digit-char-p c) (char= #\. c) (char= #\- c)))
(defun ident-start-p (c) (alpha-char-p c))
(defun ident-part-p (c) (or (alpha-char-p c) (digit-char-p c) (char= #\_)))
(defun str-start-p (c) (or (char= #\' c) (char= #\" c)))
(defun kw-start-p (c) (member c *sql-keyword-start-chars* :test 'char=))
(defun sym-start-p (c) (member c *sql-symbol-start-chars* :test 'char=))

(defun next-sql-token (stream)
  "Parse the next sql token from input STREAM else return nil."
  (block :next
    (let ((tok)
          (next (peek-char t stream nil nil)))
      (unless next
        (return-from :next tok))
      (cond
        ((num-start-p next) (make-sql-token
                             :text (format nil "~A" (read-preserving-whitespace stream))
                             :type :num
                             :end (file-position stream)))
        ((ident-start-p next) (make-sql-token
                               :text (format nil "~A" (read-preserving-whitespace stream))
                               :type :ident
                               :end (file-position stream)))
        ((str-start-p next) (make-sql-token
                             :text (format nil "~A" (read-preserving-whitespace stream))
                             :type :str
                             :end (file-position stream)))
        ((sym-start-p next) (make-sql-token
                             :text (format nil "~A" (read-preserving-whitespace stream))
                             :type :sym
                             :end (file-position stream)))
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
(defclass sql-parser (pratt-parser)
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
  (let ((token (car (sql-tokens self))))
    (unless (null token)
      (case (sql-token-type token)
        (:kw (string-case ((sql-token-text token))
               ("SELECT" nil)
               ("CAST" nil)
               ("MAX" (make-instance 'sql-identifier :id "MAX"))
               ("INT" (make-instance 'sql-identifier :id "INT"))
               ("DOUBLE" (make-instance 'sql-identifier :id "DOUBLE"))))
        (:ident (make-instance 'sql-identifier :id (sql-token-text token)))
        (:str (make-instance 'sql-string :value (sql-token-text token)))
        (:num (make-instance 'sql-number :value (parse-number (sql-token-text token))))))))

(defmethod parse-infix ((self sql-parser) (left sql-expression) precedence)
  (let* ((tokens (sql-tokens self))
         (token (car tokens)))
    (unless (null token)
      (case (sql-token-type token)
        (:sym (cond
                ((member (sql-token-text token) (list (get-sql-symbol :PLUS) (get-sql-symbol :SUB)
                                                      (get-sql-symbol :STAR) (get-sql-symbol :SLASH)
                                                      (get-sql-symbol :EQ) (get-sql-symbol :GT)
                                                      (get-sql-symbol :LT))
                         :test 'string=)
                 (pop tokens) ;; consume
                 (make-instance 'sql-binary-expr
                   :lhs left
                   :op (sql-token-text token)
                   :rhs (parse self precedence)))
                ((string= "(" (sql-token-text token))
                 (pop tokens)
                 (let ((args (parse-expression-list self)))
                   (assert (string= (sql-token-text (pop tokens)) ")"))
                   (make-instance 'sql-function :id (id left) :args args)))
                (t nil)))
        (:kw (string-case ((sql-token-text token))
               ("AS" (pop tokens)
                     (make-instance 'sql-alias
                       :expr left
                       :alias (parse-identifier self)))
               ("AND" (pop tokens)
                      (make-instance 'sql-binary-expr
                        :lhs left
                        :op "AND"
                        :rhs (parse self precedence)))
               ("OR" (pop tokens)
                     (make-instance 'sql-binary-expr
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
               (etypecase (sql-token-type sort)
                 (sql-identifier (setf sort (make-instance 'sql-sort :expr sort :asc t)))
                 (sql-sort nil))
               (push sort sort-list)
               (let ((next (car (sql-tokens self))))
                 (when (and (eql (sql-token-type next) :sym) (string= (sql-token-text next) ","))
                   (pop (sql-tokens self)))
                 (setf sort (parse-expression self))))
          finally (return sort-list))))

(defmethod parse-cast ((self sql-parser))
  (let ((tokens (sql-tokens self)))
    (assert (string= (sql-token-text (pop tokens)) "("))
    (let* ((expr (parse-expression self))
           (alias (make-instance 'sql-alias :expr expr)))
      (assert (string= (sql-token-text (pop tokens)) ")"))
      (make-instance 'sql-cast :expr expr :type (slot-value alias 'alias)))))

(defmethod parse-select ((self sql-parser))
  (let ((projection (parse-expression-list self))
        table filter-expr group-by having-expr order-by 
        (tok (pop (sql-tokens self))))
    (ecase (sql-token-type tok)
      (:kw (string-case ((sql-token-text tok))
             ("FROM" (setf table (parse-expression self))
                     ;; ...
                     ))))
    (make-instance 'sql-select
      :projection projection
      :filter filter-expr
      :group-by group-by
      :order-by order-by
      :having having-expr
      :table-name (id table))))

(defmethod parse-expression-list ((self sql-parser))
  (let ((lst)
        (expr (parse-expression self)))
    (loop while expr
          do (push expr lst)
          finally (return lst))))

(defmethod parse-expression ((self sql-parser))
  (parse self 0))

(defmethod parse-identifier ((self sql-parser))
  (let ((expr (parse-expression self)))
    (if (typep expr 'sql-identifier)
        expr
        (simple-sql-error))))

(defmacro with-sql-parser ((sym &optional tokens) &body body)
  `(let ((,sym (make-instance 'sql-parser :tokens ,tokens)))
     (print ,sym)
     ,@body))
