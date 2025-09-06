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

(define-condition syntax-condition () ((ast :initarg :ast :initform nil :accessor ast)))

(eval-always
  (deferror syntax-error (syntax-condition error) ())
  (defwarning syntax-warning (syntax-condition warning) ())
  (deferror invalid-ast (syntax-error)
    ((ast :initform nil :initarg :ast :accessor ast))
    (:report (lambda (c s)
               (format s "Invalid Skel AST: ~A" (ast c))))))

(defun invalid-ast (ast)
  (error 'invalid-ast :ast ast))

(defvar *ast* nil)

(defvar *keep-ast* nil
  "Whether to keep the :ast slot stored with an object, or set it to nil so
that it can be GC'd.")

(defun formp (form)
  (or (consp form) (atom form)))

(deftype form ()
  '(satisfies formp))

;;; NODE objects

;; The 'DEF*' macros defined here are from C-MERA.

;; Symbols in 'subnodes' describe slots that contain nodes.
;; Slots with only atoms are listed in 'values'.
(defclass node () ())

(defmacro defnode (name supers slots &rest opts)
  "Define a new subclass of NODE."
  `(defclass! ,name ,(safe-superclasses 'node supers) ,slots ,@opts))

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
  (declare (class class)
           (form form)
           (ignore class form)))

;;; AST Object
(defclass ast (node)
  ((ast :initarg :ast :accessor ast)))

(defgeneric build-ast (self &key &allow-other-keys)
  (:documentation "Build an AST of SELF and store it in the :ast
slot.")
  (:method ((self ast) &key (nullp nil) (exclude '(ast id)))
    (setf (ast self)
          (unwrap-object self
                         :slots t
                         :methods nil
                         :nullp nullp
                         :exclude exclude))
    self))

(defgeneric load-ast (self)
  (:documentation "Load the object SELF from the :ast slot.")
  (:method ((self ast))
    (with-slots (ast) self
      (sb-int:doplist (k v) ast
        (setf (slot-value self k) v)))))

(defgeneric load-ast* (self context)
  (:documentation "load the object SELF from the :ast slot with additional CONTEXT."))

(defgeneric wrap (self form)
  (:documentation "Wrap object FORM using SELF, usually sets the AST slot.")
  (:method ((self ast) form) (setf (slot-value self 'ast) form)))

(defgeneric unwrap (self)
  (:documentation "Unwrap object SELF, usually returns the AST slot.")
  (:method ((self ast)) (slot-value self 'ast)))

;;; AST Traversal
(defclass debug-traverser () ())

(defclass copy-traverser ()
  ((stack :initform '())
   (result :initform nil)))

(defgeneric traverse (self node level)
  (:method ((self t) (node node) level)
    (if (slot-exists-p node 'ast)
        (loop for i in (ast node)
              do (traverse self i (1+ level)))
        (call-next-method)))
  (:method ((self t) (node ast) level)
    (with-slots (ast) node
      (mapcar (lambda (x) (traverse self x level)) ast)))
  (:method ((self t) (node list) level)
    (mapcar (lambda (x) (traverse self x level)) node))
  (:method ((self t) (item t) level)
    (declare (ignore level)))
  (:method ((self debug-traverser) (node t) level)
    (format *trace-output* "~&traverse:  ~A~%" (class-name (class-of node))))
  (:method :before ((copy copy-traverser) (item node) level)
    (declare (ignore level))
    (with-slots (stack) copy
      (push '() stack)))
  (:method :after ((copy copy-traverser) (item node) level)
    (with-slots (stack result) copy
      (with-slots (values subnodes) item
        (let ((node-type (class-of item)))
          (let ((node-copy nil)
                (subnodes subnodes) ; changes can occur
                (subnode-copies (reverse (pop stack))))
            (if (eq node-type (find-class 'nodelist))
                (setf node-copy (make-instance 'nodelist
                                  :nodes subnode-copies
                                  :values '()
                                  :subnodes '(nodes)))
                (progn
                  (setf node-copy (allocate-instance node-type))
                  (dolist (slot (mapcar #'sb-pcl::slot-definition-name 
                                        (sb-pcl::class-slots node-type)))
                    (when (slot-boundp item slot)
                      (when (eq (slot-value item slot) nil)
                        (setf subnodes (remove slot subnodes))) 
                      (let ((position (position slot subnodes)))
                        (setf (slot-value node-copy slot)
                              (if position
                                  (nth position subnode-copies)
                                  (slot-value item slot))))))))
            (if (eq level 0)
                (setf result node-copy)
                (push node-copy (first stack)))))))))

;;; EXPRESSION Objects
(defgeneric op (self))
(defgeneric lhs (self))
(defgeneric (setf lhs) (new self))
(defgeneric rhs (self))
(defgeneric (setf rhs) (new self))

(defclass expr (node) ()
  (:documentation "Base Expression Object."))

(defmacro defexpr (name supers slots &rest opts)
  `(defclass! ,name ,(safe-superclasses 'expr supers) ,slots ,@opts))

(defclass literal-expr (expr) 
  ((val :initarg :val :accessor val)))
(defmethod ast ((self literal-expr)) (val self))
(defclass logical-expr (expr) ())
(defclass physical-expr (expr) ())

(defclass unary-expr (expr)
  ((expr :initarg :expr :accessor expr)))
(defmethod ast ((self unary-expr)) (val self))
(defclass binary-expr (expr)
  ((lhs :initarg :lhs :accessor lhs)
   (rhs :initarg :rhs :accessor rhs)))
(defmethod ast ((self binary-expr)) (list (lhs self) (rhs self)))
;;; Statements
(defclass stmt (node) ())

(defmacro defstmt (name supers slots &rest opts)
  `(defclass! ,name ,(safe-superclasses 'stmt supers) ,slots ,@opts))

;;; Read/Write
(defgeneric read-ast (self stream)
  (:method ((self ast) stream)
    (setf (ast self) (read-lisp-until-end stream)))
  (:method ((self ast) (stream pathname))
    (read-ast self (open stream)))
  (:method ((self ast) (stream string))
    (read-ast self (open stream))))

(defgeneric write-ast (self stream &key)
  (:method ((self ast) stream &key pretty case)
    (with-open-stream (st stream)
      (flet ((.write (x) (write x :stream st :pretty pretty :case case :readably t :array t :escape t)))
        (if pretty
            (loop for (k v . rest) on (ast self)
                  by #'cddr
                  do
                     (.write k)
                     (write-char #\space st)
                     (typecase v
                       (ast (write-ast v st :pretty pretty :case case))
                       (t (.write v)))
                     (write-char #\newline st))
            (.write (ast self))))))
  (:method ((self ast) (stream pathname) &rest args)
    (apply 'write-ast self (open stream) args))
  (:method ((self ast) (stream string) &rest args)
    (apply 'write-ast self (open stream) args)))

(defun read-ast-string (self str) (with-input-from-string (s str) (read-ast self s)))

(defun write-ast-string (self) 
  (let ((ast (ast:ast self)))
    (declare (list ast))
    (if (> (length ast) 1)
        (write-to-string ast)
        (write-to-string (car ast)))))

;;; Printer

;; primitive support for printing AST Nodes is provided here and implemented
;; by higher-level packages. We use the Pretty Printer machinery as much as
;; possible.

;; ref: https://dl.acm.org/doi/pdf/10.1145/1039991.1039996

;;; Write
(defvar *ast-dispatch-table* (copy-pprint-dispatch))
(defun pprint-ast (sexpr &rest args)
  (apply 'write sexpr :pretty t :pprint-dispatch *ast-dispatch-table* args))
