;;; ast.lisp --- GEN/RS AST Nodes

;; 

;;; Code:
(in-package :syn/gen/rs)

;;; Nodes
(defstmt rs-comment (comment) ())
(defnode trait () ())
(defnode rs-type (c-type) ())
(defexpr raw-str-literal (literal-expr) ())
(defexpr boolean-literal (literal-expr) ())
(defexpr byte-literal (literal-expr) ())
(defexpr byte-str-literal (literal-expr) ())
(defexpr byte-str-raw-literal (literal-expr) ())
(defexpr c-str-raw-literal (literal-expr) ())
(defexpr c-str-literal (literal-expr) ())
(defexpr err-literal (literal-expr) ())
(defexpr match (prefix-expression) ())
(defexpr range () ())

;;; Context Switches
(build-context-switches
 :package :syn/gen/rs/sym
 :symbols *rs-symbols*)

(build-swap-package
 :package :syn/gen/rs/sym
 :swap-package :syn/gen/rs/swap
 :symbols *rs-swap*)

;;; Syntax
(defmacro rs-syntax (tags lambda-list &body body)
  `(defsyntax ,tags (:rs) ,lambda-list ,@body))

;; binary ops
(rs-syntax (<= >= < > + - * / = || && == != % << >> ^ | & += /= *= %= >>= <<= -= |= &= ^=) 
    (&rest rest)
  "Infix expressions for multiple inputs"
  `(make-instance 'infix-expression :op ',syn/gen::tag :members (make-nodes ,rest)))

;; unary ops
(rs-syntax (* ! -) (&rest rest)
  (if (eql (length rest) 1)
      `(make-instance 'prefix-expression :op ',syn/gen::tag :object (make-node ,@rest))
      `(make-instance 'infix-expression :op ',syn/gen::tag :members (make-nodes ,rest))))
