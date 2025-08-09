;;; ast.lisp --- APL AST Nodes

;; 

;;; Commentary:

;; IMPORTANT NOTE: Evaluation order of APL expressions is RIGHT to LEFT. 

;; The argument order is inverted when Lisp parses it so A + B isn't (+ A B),
;; it is (+ B A). You will see in the definitions below the right operand
;; comes before the left.

;;; Code:
(in-package :syn/apl)

(defexpr apl-op (binary-expr) (op))
(defexpr monadic-function (unary-expr) (op))
(defexpr dyadic-function (binary-expr) (op))

;; structure = (rank shape depth)
(deftype apl-array-type () `(member number character t))

(defconstant +zilde+ :⍬)

(defexpr array-expression () (structure type array))
(defexpr vector-expression () (type vector))
(defexpr variable-expression () (variable value))

(defmacro apl-syntax (tags lambda-list &body body)
  `(syn/gen:defsyntax ,tags (:apl) ,lambda-list ,@body))

(apl-syntax (/ ⌿ \ ⍀) (r l)
  "APL Operator or Function. Argument to the left determines type."
  `(make-instance 'apl-op :op ',syn/gen::tag :lhs ,l :rhs ,r))

(apl-syntax (¨ ⍤ ⍥ ⌸ ⌺ ⍨ ⍣ |.| ∘ ⍠ @ & ⌶) (l r)
  "APL Operator"
  `(make-instance 'apl-op :op ',syn/gen::tag :lhs ,l :rhs ,r))

(apl-syntax (∧ ∨ ⍲ ⍱ < ≤ = ≥ > ⍷ ⊤ ⊥ ∩) (r l)
  "Dyadic-only Functions"
  `(make-instance 'dyadic-function :op ',syn/gen::tag :lhs ,l :rhs ,r))

(apl-syntax (+ -) (r &optional l)
  "Monadic or Dyadic Function"
  (if l
      `(make-instance 'dyadic-function :op ',syn/gen::tag :lhs ,l :rhs ,r)
      `(make-instance 'monadic-function :op ',syn/gen::tag :expr ,r)))

#|
The Structural functions are reshape (⍴), ravel, laminate and catenate (,),
reverse and rotate (⌽), transpose (⍉), mix and take (↑), split and drop (↓), enlist (∊),
and enclose (⊂).
|#
