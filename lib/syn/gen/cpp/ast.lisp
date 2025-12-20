;;; ast.lisp --- CPP AST Nodes

;; 

;;; Code:
(in-package :syn/gen/cpp)

(defnode superclass () (attribute superclass))
(defnode declaration-list-initializer () (list-items))
;; pure virtual
(defnode function-definition () (pure virtual item parameter tail-qualifiers body))

(defstmt class () (name superclasses body))
(defstmt constructor () (name parameter initializer body))
;; virtual
(defstmt destructor () (virtual name body))
(defstmt access-specifier (specifier) (body))
(defstmt namespace () (namespace body))
(defstmt using-namespace () (namespace))
(defstmt using () (item))
(defstmt template () (parameters body))
(defstmt try-block () (body catches))
;; all
(defstmt catch () (all decl-item body))

(defexpr from-namespace () (namespace name))
(defexpr instantiate () (template arguments))
(defexpr instantiate-explicit () (item))
;; operator
(defexpr new () (operator specifier type))
;; operator
(defexpr delete () ( operator object))
(defexpr lambda-definition () (capture parameter tail-qualifiers type body))

;;; Syntax
(defmacro cpp-syntax (tags lambda-list &body body)
  `(defsyntax ,tags (:cpp) ,lambda-list ,@body))

(defmacro decompose-superclass (item)
  ;; build superclass node
  `(superclass
    ;; access specifier
    (make-node ,(first item))
    ;; class name
    (make-node ,(second item))))

(defmacro decompose-initializer (item)
  ;; build initalizer calls
  `(funcall
    ;; initializer name
    (make-node ,(first item))
    ;; initialize with
    (make-node ,(second item))))

(defun decompose-declaration (item)
  "Decompose initializer list / quite like declaration item. The last
   value returnd specifies if the declaration actually used an
   initializer list or not."
  ;; check if initialization is present
  (let ((val (first (last item))))
    (if (and (listp val)
             (eql (length val) 1)
             (listp (car val))
             (eql (length (car val)) 1)
             (listp (caar val)))
        ;; decompose arg list with list initializer
        (let ((spec+type+id (butlast item))
              (inits        (caar val)))
          (let ((specifier (butlast spec+type+id 2))
                (type+id   (last    spec+type+id 2)))
            (values specifier (first type+id) (second type+id) inits t)))
        ;; pass to standard declaration decomposition
        (multiple-value-bind (spec type name init) (syn/gen/c:decompose-declaration item)
          (values spec type name init nil)))))

(defmacro make-declaration-node-with-list (item)
  "Decompose initializer list and instantiate nodes / quite like declaration item"
   (multiple-value-bind (specifier type id init initializer-list-p)
       (decompose-declaration item)
    `(declaration-item
      ;; set specifiers
      ,(when specifier
             `(specifier
               (make-nodes ,specifier)))
      ;; set type
      (type (make-node ,type))
      ;; set identifier
      (make-node ,id)
      ;; set value
      ,(if init 
           (if initializer-list-p
               `(declaration-list-initializer (make-nodes ,init))
               `(declaration-value (make-node ,init)))
           nil))))

(cpp-syntax decl (bindings &body body)
  "Declare variables"
  `(declaration-list
    ;; braces t, adjusted later by traverser
    t
    ;; make single declarations/bindings
    (make-nodes
     ,(remove nil bindings) :prepend make-declaration-node-with-list)
    ;; make listnode with body
    ,(when body
         ;; make single expression statements
       `(make-exprs ,body))))

;; function, lambda funciton helper
(defun symbol-name-in-list (symbol list)
  (member-if (lambda(x) (when (symbolp x) (equal (symbol-name x) (string-upcase symbol)))) list))

;; copy/extend  for pure virtual functions
(cpp-syntax function (name parameters &rest rest &environment env)
  "Define c++ function"
    (let ((qualifiers (reverse (rest (symbol-name-in-list "->" (reverse rest))))))
      (destructuring-bind (type &body body) (rest (symbol-name-in-list "->" rest))
        (flet ((qualifier-there (Q)
                 (let ((there (symbol-name-in-list Q qualifiers)))
                   (if there
                       (setf qualifiers (remove-if (lambda (x) (eql x (first there)))
                                                   qualifiers)))
                   (if there t nil))))
          (let* ((pure    (qualifier-there "pure"))
                 (virtual (or (qualifier-there "virtual") pure)))
            `(function-definition
              ,pure
              ,virtual
              ;; function name + type
              ,(if (listp type)
                   ;; check if macro/function or list
                   (let ((first (first type)))
                     (if (and (not (listp first)) (std:fboundp! first env))
                         ;; type is macro or function
                         `(make-declaration-node (,type ,name))
                         ;; type is list with type information
                         `(make-declaration-node (,@type ,name))))
                   ;; type is single symbol
                   `(make-declaration-node (,type ,name)))
              ;; parameter list
              (parameter-list
               (make-nodes ,parameters :prepend make-declaration-node))
              ,(if qualifiers
                   `(specifier (make-nodes ,qualifiers))
                   nil)
              ;; body
              ,(if pure
                   `(syn/gen/c/sym::set nil 0)
                   (when body
                     `(make-block ,body)))))))));)

(cpp-syntax lambda-function (capture parameters &rest rest &environment env)
  "Define c++11 lambda function"
  (let ((qualifiers (reverse (rest (symbol-name-in-list "->" (reverse rest)))))
        (ret-body (rest (symbol-name-in-list "->" rest)))
        (body rest)
        (type nil))
    (when ret-body
      (setf body (rest ret-body))
      (setf type (first ret-body)))
    `(lambda-definition
       ;; caputre
       (parameter-list
         (make-nodes 
           ;; check if macro/function or list
           ,(loop for i in capture collect
             (if (and (listp i) (not (std:fboundp! (first i) env)))
                 ;; element is is simple list and not bound 
                 i
                 ;; element is bound
                 `(,i)))
           :prepend make-declaration-node))
       ;; parameter
       (parameter-list
         (make-nodes ,parameters :prepend make-declaration-node))
       ;; qualifiers
       ,(if qualifiers
          `(specifier (make-nodes ,qualifiers))
          nil)
       ;; return type
       ,(if type
            (if (listp type)
                ;; chedk if macro/funciton or list
                (let ((First (first type)))
                       (if (and (not (listp first)) (std:fboundp! first env))
                         ;; type is macro or function
                         `(make-declaration-node (,type nil))
                         ;; type is list with type information
                         `(make-declaration-node (,@type nil))))
                ;; type is single symbol
                `(make-declaration-node (,type nil))) ;; no name -> nil
            nil)
       ;; body
       ,(when body
          `(make-block ,body)))))

(cpp-syntax constructor (name args &body body)
  "Constructor with initializer list"
  (let ((initializer nil))
    (when (eql (first body) :init)
      (setf initializer (second body))
      (setf body (rest (rest body))))
    `(constructor
         ;; constructor name
         (make-node ,name)
       ;; parameter
       (parameter-list
        (make-nodes ,args :prepend make-declaration-node))
       ;; initializer
       ,(when initializer
              `(make-nodes ,initializer))
       ;; body
       ,(when (or body initializer)
              `(make-block ,body)))))

(cpp-syntax destructor (name &body body)
  "Destructor"
  (let* ((first (first body))
         (virtual (and (listp body) (symbolp first) (equal (symbol-name first) "VIRTUAL")))
         (body    (if virtual (rest body) body)))
    `(destructor
       ,virtual
       ;; destructor name
       (make-node ,name)
       ;; body
       ,(when body
          `(make-block ,body)))))

(defun wrap-statements (list)
  "Wrap all elements in list in expression-statement and
   access-specifier, apply quoty and combine into AST"
   `(make-nodes 
      ,(loop for i in list collect 
         `(access-specifier nil
            (expression-statement nil
              (quoty ,i))))))

(cpp-syntax class (name superclasses &body body)
  "Define a c++ class with c'tor and d'ctor mactoler"
  ;; macrolet for locally defined c'tor and d'tor
  `(macrolet ((syn/gen/cpp/sym::constructor (args &body body)
    `(syn/gen/cpp/swap::constructor ,',name ,args ,@body))
        (syn/gen/cpp/sym::destructor (&body body)
    `(syn/gen/cpp/swap::destructor ,',name ,@body)))
     (class
      ;; class name
      (make-node ,name)
      ;; superclasses
      (make-nodes
       ,superclasses :prepend decompose-superclass)
      ;; compund statement with individual expr statements
    ,(if body
     `(compound-statement
      ;; curly braces
      t 
      ;; prepared body
      ,(wrap-statements body))
     nil))))

(cpp-syntax struct (name &body body)
  "Struct redefinition, required for access specifiers"
  `(struct-definition
   ;; struct name
   (make-node ,name)
   ,(when body
    `(compound-statement
       ;; curly braces
       t
       ;; modified body
       ,(wrap-statements body)))))

(cpp-syntax (private public protected) (&body body)
  "Class access specifier"
  `(access-specifier ',tag ,(wrap-statements body)))

(cpp-syntax namespace (namespace &body body)
  "Make new namespace"
  `(namespace
    ;; namespace name
    (make-node ,namespace)
    ;; make namespace body
    (make-block ,body)))

(cpp-syntax using (item)
  "Using something"
  `(using (make-node ,item)))

(cpp-syntax using-namespace (item)
  "Using namespace"
  `(using-namespace (make-node ,item)))

(cpp-syntax (new new[]) (&rest object)
  "Make new object"
  (let ((specifier (butlast object))
        (object (first (last object))))
    `(new
      ;; new / new[]
      ',tag
      ;;specifier
      ,(when specifier
         `(specifier
           (make-nodes ,specifier)))
      ;; type/object
      (make-node ,object))))

(cpp-syntax (delete delete[]) (item)
  "Delete object"
  `(delete
    ;; delete / delete[]
    ',tag
    (make-node ,item)))

(cpp-syntax c-throw (item)
  "Throw is just a jump statement"
  `(jump-statement
    (make-node ,tag)
    ,(when item `(make-node ,item))))

(defmacro make-catch-decl-item ((args &body body))
  (let ((all (eq args t)))
    `(catch
       ,all
       ,(if (not all)
            `(make-declaration-node ,args))
       (make-block ,body))))

(cpp-syntax catching (clauses &body body)
  `(try-block
     (make-block ,body)
     (make-nodes ,clauses :prepend make-catch-decl-item)))

(cpp-syntax from-namespace (&rest rest)
  "From namesapce ::foo // foo::bar"
  ;; set last item
  (let ((namespace-cascade `(make-node ,(first (last rest)))))
    ;; loop form back to front collect namespaces
    (loop for i in (rest (reverse rest)) do
      (setf namespace-cascade
            `(from-namespace
              (make-node ,i)
              ,namespace-cascade)))
    namespace-cascade))

(cpp-syntax template (parameters item)
  "C++ templates"
  `(template
    ;; set parameters
    (parameter-list
     (make-nodes ,parameters :prepend make-declaration-node))
    ;; body
    (make-node ,item)))

(cpp-syntax instantiate (name &rest arguments)
  "Intantiate template"
  `(instantiate
    ;; name
    (make-node ,name)
    ;;
    (make-nodes
     ,arguments :prepend syn/gen/c:decompose-type)))

(cpp-syntax instantiate-explicit (item)
  "Explicit template instantiateio"
  `(instantiate-explicit ,item))

(cpp-syntax reference-type (item)
  "Postfix & operator (reference"
  `(postfix-expression '& (make-node ,item)))

(cpp-syntax for (init &body body)
  "Similar to c version but with foreach support"
  `(for-statement
    ;; check if initialization present
    ,(when (first init)
     ;; set init
     `(make-declaration-node ,(first init)))
    ;; test / foreach container
    (make-node ,(second init))
    ;; check if step present
    ,(when (third init)
     ;; set step
     `(make-node ,(third init)))
    ;; the loop body
    (make-block ,body)))
