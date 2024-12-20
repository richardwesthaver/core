;;; ast.lisp --- GEN/RS AST Nodes

;; 

;;; Code:
(in-package :syn/gen/rs)

;;; Nodes
(defstmt rs-comment (comment) ())
(defnode trait () ())
(defnode rs-type (c-type) ())
(defexpr raw-str-literal (literal-expr) ())

;;; Syntax
(defmacro rs-syntax (tags lambda-list &body body)
  `(defsyntax ,tags (:rs) ,lambda-list ,@body))

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

(rs-syntax (<= >= < > + - * / = || && == != % << >> ^ | & += /= *= %= >>= <<= -= |= &= ^=) 
    (&rest rest)
  "Infix expressions for multiple inputs"
  `(make-instance 'infix-expression :op ',syn/gen::tag :members (make-nodes ,rest)))
