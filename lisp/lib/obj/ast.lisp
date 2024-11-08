;;; ast.lisp --- Abstract Syntax Trees

;; AST Objects

;;; Commentary:

;; This package was initially isolated to the OBJ/QUERY AST machinery, but as
;; we are getting into codegen for C/CUDA/etc we now have a need to generalize
;; it into a unique package.

;; The objects in this package are probably not that useful in a Lisp-only
;; context, or even a Lisp -> Lisp transpiler where we can leverage
;; homoiconicity.

;; These objects are best suited for a Lisp <-> Non-Lisp environment - where
;; we want to be able to parse some Non-Lisp target language and write Lisp
;; that emits code in that language.

;; This package is depended on by OBJ/QUERY and Q for SQL and the like, as
;; well as SYN/GEN which contains support for our Non-Lisp programming
;; languages.

;;; Code:
(in-package :obj/ast)

(defvar *ast* nil)
(defun formp (form)
  (or (consp form) (atom form)))

(deftype form ()
  '(satisfies formp))

(defgeneric build-ast (self &key &allow-other-keys)
  (:documentation "Build an AST of SELF and store it in the :ast
slot."))

(defgeneric load-ast (self)
  (:documentation "Load the object SELF from the :ast slot."))

(defgeneric load-ast* (self context)
  (:documentation "load the object SELF from the :ast slot with additional CONTEXT."))

(defgeneric wrap (self form)
  (:documentation "Wrap object FORM using SELF, usually sets the AST slot."))

(defgeneric unwrap (self)
  (:documentation "Unwrap object SELF, usually returns the AST slot."))
(defgeneric (setf unwrap) (new self))

;;; AST Object
(defclass ast ()
  ((ast :initarg :ast :accessor ast)))

(defmethod wrap ((self ast) form) (setf (slot-value self 'ast) form))

(defmethod unwrap ((self ast)) (slot-value self 'ast))

;;; WRAP-OBJECT/UNWRAP-OBJECT
(declaim (inline unwrap-object)) ;; inline -200
(defun unwrap-object (obj &key (slots t) (methods nil)
                            (indirect nil) (tag nil)
                            (unboundp nil) (nullp nil)
                            (exclude nil))
  "Build and return a new `form' from OBJ by traversing the class
definition. This differs from the generic function `unwrap' which
always uses the ast slot as an internal buffer. We can also call this
on any class instance (doesn't need to subclass AST).

SLOTS specifies the slots to be included in the output. If the value
is t, all slots are included. The ast slot is not included by default,
but this behavior may change in future revisions.

When INDIRECT is non-nil, also include methods which indirectly
specialize on OBJ.

When TAG is non-nil, return a cons where car is TAG and cdr is the
output. If TAG is t, use the class-name symbol."
  (declare (type standard-object obj)
           (type (or list boolean) slots)
           (type (or list boolean) methods)
           (type boolean indirect)
           (type list exclude))
  (unless (or slots methods)
    (error "Required one missing key arg: SLOTS or METHODS"))
  (let* ((class (class-of obj))
         (res (when tag (list (if (eq t tag) (class-name class) tag)))))
    (block unwrap
      (when-let ((slots (when slots
                          (list-class-slots class slots exclude))))
        (let ((slot-vals (list-slot-values-using-class class obj (remove-if #'null slots) nullp unboundp)))
          (if methods
              (push slot-vals res)
              (return-from unwrap (push slot-vals res)))))
      (when-let ((methods (when methods (list-class-methods class methods indirect))))
        (push methods res)))
    (flatten res)))

;; TODO 2024-03-22: 
(defun wrap-object (class form)
  "Given a CLASS prototype and an input FORM, return a new instance of
CLASS. FORM is assumed to be the finalized lisp object which has
already passed through `read' -- not a string or file-stream for
example."
  (declare (type class class)
           (type form form)))

;;; EXPRESSION Objects
(defgeneric expr-name (self))
(defgeneric expr-op (self))
(defgeneric lhs (self))
(defgeneric (setf lhs) (new self))
(defgeneric rhs (self))
(defgeneric (setf rhs) (new self))

(defclass expr () ()
  (:documentation "Base Expression Object."))

(defclass literal-expr (expr) ())
(defclass logical-expr (expr) ())
(defclass physical-expr (expr) ())

(defclass unary-expr (expr)
  ((expr :initarg :expr :accessor expr)))

(defclass binary-expr (expr)
  ((lhs :initarg :lhs :accessor lhs)
   (rhs :initarg :rhs :accessor rhs)))
