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

(defnode rs-function-definition (gen/c::function-definition) ())
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

;; FIX 2025-03-06: 
(rs-syntax fn (name params type &body body &environment env)
  "Define a Rust function"
  `(make-instance 'syn/gen/c::function-definition
     ;; function name + type
     :item ,(if (listp type)
	        (let ((first (first type)))
	          (if (and (not (listp first)) (std:fboundp! first env))
	              ;; type is macro or function
	              `(gen/c::make-declaration-node (,type ,name))
	              ;; type is list with type information
	              `(gen/c::make-declaration-node (,@type ,name))))
                `(gen/c::make-declaration-node (,name ,type)))
                
     :parameter
     (make-instance 'syn/gen/c::parameter-list
       :parameters
       (make-nodes ,params :prepend syn/gen/c::make-declaration-node))
     :body
     ,(when body
	`(make-block ,body))))

