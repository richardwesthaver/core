;;; ast.lisp --- SYN/GEN/C AST Nodes

;; 

;;; Code:
(in-package :syn/gen/c)

(defnode function-definition () (item parameter body))
(defmethod ast ((self function-definition)) 
  (list 
   ;; params are before the result type (item) in the AST
   (slot-value self 'item)
   (slot-value self 'parameter)
   (slot-value self 'body)))
(defnode parameter-list () (parameters))
(defmethod ast ((self parameter-list))
  (ast (slot-value self 'parameters)))
;; struct
(defnode struct-definition (id) (members))
;; union
(defnode union-definition (id) (members))
;; enum
(defnode enum-definition (id) (members))
(defmethod ast ((self enum-definition)) (list (id self) (slot-value self 'members)))
;; variable declaration
(defnode declaration-list () (braces bindings body))
(defmethod ast ((self declaration-list))
  (list (slot-value self 'bindings)
        (slot-value self 'body)))
(defnode declaration-item () (specifier type identifier value))
(defmethod ast ((self declaration-item))
  (list (slot-value self 'type)
        (slot-value self 'specifier)
        (slot-value self 'identifier)
        (slot-value self 'value)))
(defnode declaration-value () (value))
(defmethod ast ((self declaration-value)) 
  (list (slot-value self 'value)))
(defmethod val ((self declaration-value))
  (slot-value self 'value))
;; essential bulding blocks
(defnode clist () (items))
(defnode array-reference  () (array indizes))
(defnode object-reference  () (object component))
(defnode pointer-reference  () (pointer component))
(defnode c-type  () (type))
(defmethod ast ((self c-type)) (list (slot-value self 'type)))
(defnode float-type () (number))
(defnode specifier () (specifier))
(defmethod ast ((self specifier)) (list (slot-value self 'specifier)))
(defnode function-pointer () (identifier parameters))

;;; Expressions
;; = *= /= %= += -= <<= >>= &= ^= \|=
(defexpr assignment-expression () (op variable value))
(defmethod ast ((self assignment-expression))
  (list (slot-value self 'variable) (slot-value self 'value)))
;; + - / * > <  == != += -= >= <= \| \|\| & &= && % << >> or and
(defexpr infix-expression () (op members))
(defmethod ast ((self infix-expression))
  (slot-value self 'members))
;; - + -- ++ ! * &
(defexpr prefix-expression () (op object))
;; - + -- ++ *
(defexpr postfix-expression () (op object))
;; not ('!' defined as prefix)
(defexpr not-expression () (value))
;; ? : 
(defexpr conditional-expression () (test then else))
;; cast
(defexpr cast-expression () (type object))

;;; Statements
;; goto, continue, break return
(defstmt jump-statement () (kind members))
(defmethod ast ((self jump-statement))
  (ast (slot-value self 'members)))
(defstmt label-statement () (name))
(defstmt expression-statement () (force-semicolon expression))
(defstmt compound-statement () (braces statements))
(defstmt if-statement () (test if-body else-body))

;;; Loops
(defstmt for-statement () (init test step body))
(defstmt while-statement () (test body))
(defstmt do-statement () (body test))

;;; comment
(defstmt comment () (chars comment linebreak))

;;; switch
(defstmt switch-case-statement () (switch cases))
(defnode switch-case-item () (constant body))

;;; gcc ext
(defexpr attribute-expression () (arguments))

;;; typedef
(defstmt typedef () (declaration))
;;; special nodes
(defnode include () (file))
(defmethod ast ((self include)) (list (slot-value self 'file)))
;; TODO 2024-12-13: 
(defnode preprocessor-macro () (name function body))

;;; Syntax
(defmacro c-syntax (tags lambda-list &body body)
  `(defsyntax ,tags (:c) ,lambda-list ,@body))

(defmacro make-exprs (list)
  `(make-nodes ,list 
               ;; :prepend (make-instance 'expression-statement)
               :quoty t))

(defmacro make-block (list)
  "Code block with curly braces and indentation."
  `(make-instance 'compound-statement 
     :braces t
     :statements (make-exprs ,list)))

(defmacro make-simple-block (list)
  "Code block without underlying AST.
   Used for 'bodys' where implicit progn is required"
  `(make-instance 'compound-statement
     :braces t
     :statements (make-instance 'expression-statement 
                   :force-semicolon nil 
                   :expression (quoty ,list))))

(c-syntax block (&body body)
  "Code block with curly braces and indentation."
  `(make-block ,body))

(c-syntax progn (&body body)
  "Code block without curly braces nor intendation"
  ;; make expressions with ';' delimiter
  `(make-exprs ,body))

(c-syntax set (&rest rest)
  "Assigment operator for multiple inputs"
  (when (oddp (length rest))
    (error "Set operator with odd number of elements: ~a" rest))
  (if (eql (length rest) 2)
      ;; signel assignment
      `(make-instance 'assignment-expression 
         :op '= 
         :variable (make-node ,(pop rest)) 
         :value (make-node ,(pop rest)))
      ;; muliple assignments
      `(make-exprs
        ;; collect item  pairwise and emmit sigle assignments
        ,(loop while rest collect
            `(make-instance 'assignment-expression 
               :op '= 
               :variable (make-node ,(pop rest)) 
               :value (make-node ,(pop rest)))))))

(c-syntax (= *= /= %= += -= <<= >>= &= ^= \|=) (variable value)
  "Assignment operators for single inputs"
  `(make-instance 'assignment-expression :op ',syn/gen::tag :var (make-node ,variable) :val (make-node ,value)))

(c-syntax (/ > < == != >= <= \| \|\| % << >> or and ^ &&) (&rest rest)
  "Infix expressions for multiple inputs"
  `(make-instance 'infix-expression :op ',syn/gen::tag :members (make-nodes ,rest)))

(c-syntax (- + * &) (&rest rest)
  "Infix or prefix version"
  (if (eql (length rest) 1)
      `(make-instance 'prefix-expression :op ',syn/gen::tag :object (make-node ,@rest))
      `(make-instance 'infix-expression :op ',syn/gen::tag :members (make-nodes ,rest))))

(c-syntax (~ !) (item)
  "Prefix operators"
  `(make-instance 'prefix-expression :op ',syn/gen::tag :object (make-node ,item)))

(c-syntax (addr) (item)
  "Address-of function (&)"
  `(make-instance 'prefix-expression :op '& :object (make-node ,item)))

(c-syntax (deref) (item)
  "Taget-of or dereferencing pointer"
  `(make-instance 'prefix-expression :op '* :object (make-node ,item)))

(c-syntax prefix++ (item)
  "Prefix operator ++"
  `(make-instance 'prefix-expression :op '++ :object (make-node ,item)))

(c-syntax prefix-- (item)
  "Prefix operator --"
  `(make-instance 'prefix-expression :op '-- :object (make-node ,item)))

(c-syntax postfix-- (item)
  "Postfix operator --"
  `(make-instance 'postfix-expression :op '-- :object (make-node ,item)))

(c-syntax postfix++ (item)
  "Postfix operator ++"
  `(make-instance 'postfix-expression :op '++ :object (make-node ,item)))

(c-syntax postfix* (item)
  "Postfix operator *"
  `(make-instance 'postfix-expression :op '* :object (make-node ,item)))

(c-syntax struct (name &body body)
  "Struct definition"
  `(make-instance 'struct-definition
    ;; struct name
     :id (make-node ,name)
    ;; struct body
     :members 
     ,(when body
        `(make-instance 'compound-statement
           :braces t
           ;; build subnodes
           :statements (make-nodes ,body)))))

(c-syntax union (name &body body)
  "Syntax for union"
  `(make-instance 'union-definition
    ;; union name
     :id (make-node ,name)
    ;; union body
     :members (make-instance 'compound-statement
                :braces t
                :statements (make-nodes ,body))))

(c-syntax enum (name &rest enum-list)
  "Syntax for enum"
  (setf enum-list (mapcar #'(lambda (x)
                              (if (listp x)
                                  x
                                  (list x))) 
                          enum-list))
  `(make-instance 'enum-definition
     :id ,(when name
            `(make-node ,name))
     :members
     (make-nodes ,enum-list :prepend decompose-enum)))

(c-syntax (aref array) (array &rest indizes &environment env)
  "Array reference"
  (if (not indizes) 
        (setf indizes '(nil)))
  ;; make array referende
  `(make-instance 'array-reference
    ;; check if identifier / type / macro
     :array
    ,(if (listp array)
         ;; check if macro/function or list
         (let ((first (first array)))
           (if (and (not (listp first)) (std:fboundp! first env))
               ;; type is macro or function
               `(make-node ,array)
               ;; type is list with type information
               `(make-declaration-node (,@array nil))))
         ;; type is single symbol
         `(make-node ,array))
    :indizes
    (make-nodes ,indizes)))

(c-syntax oref (&rest rest)
  "Object reference"
  (let* ((items (reverse rest))
         (last-item (pop items))
         (butlast-item (pop items))
         (oref `(make-instance 'object-reference 
                  :component (make-node ,butlast-item) 
                  :object (make-node ,last-item))))
    (loop for item in items do
         (setf oref `(make-instance 'object-reference 
                       :component (make-node ,item) 
                       :object ,oref)))
    oref))

(c-syntax pref (pointer component)
  "Pointer reference"
  `(make-instance 'pointer-reference :pointer (make-node ,pointer) :component (make-node ,component)))

(c-syntax type (type)
  "C data type"
  `(make-instance 'c-type :type (make-node ,type)))

(c-syntax specifier (specifier)
  "Type specifier/qualifier"
  `(make-instance 'specifier :specifier (make-node ,specifier)))

(c-syntax include (file)
  "Include for c files"
  `(make-instance 'include :file (quoty ,file)))

(c-syntax comment (comment &key (prefix nil) (linebreak t))
  "Comment with default ('//') or user defined delimiter."
  `(make-instance 'comment
     :chars (quoty ,(if prefix prefix "//"))
     :comment (quoty ,comment)
     :linebreak ,linebreak))

(defun decompose-declaration (item)
  "Decompose declaration item into its SPECIFIERS, TYPE, NAME and INITIALIZER"
  (if (< 2 (length item))
      ;; decompose arg list with init
      (let ((specifier (butlast item 3))
            (type+id+val (last item 3)))
        (let ((type (second type+id+val))
              (id   (first type+id+val))
              (init (third type+id+val)))
          (values specifier type id init)))
      ;; decompose arg list without init
      (let ((specifier (butlast item 2))
            (type+id (last item 2)))
        (let ((type (second type+id))
              (id   (first type+id)))
          (values specifier type id nil)))))

(defmacro make-declaration-node (item)
  "Decompose declaration item and instantiate nodes"
  (if (eql item '&rest)
    `(make-node '|...|)
    (multiple-value-bind (specifier type id init) (decompose-declaration item)
      `(make-instance 'declaration-item
        ;; set specifiers
         :specifier
         ,(when specifier
            `(make-instance 'specifier
               :specifier (make-nodes ,specifier)))
         :type (make-instance 'c-type :type (make-node ,type))
         :identifier (make-node ,id)
         :value ,(if init 
                     `(make-instance 'declaration-value
                        :value (make-node ,init))
                     nil)))))

(defmacro decompose-type (item)
  "Decompose type like declaration but without name"
  `(make-declaration-node (,@item nil)))

(defmacro decompose-enum (item)
  "Decompose enum like declaration but without type"
  `(make-instance 'declaration-item
    ;; no specifier
     :specifier nil
    ;; no type
     :specifier nil
    ;; enum name
     :identifier (make-node ,(first item))
    ;; enum init
     :value ,(when (second item)
               `(make-instance 'declaration-value
                  :value (make-node ,(second item))))))

(c-syntax decl (bindings &body body)
  "Declare variables"
  `(make-instance 'declaration-list
    ;; braces t, adjusted later by traverser
     :braces t
     ;; make single declarations/bindings
     :bindings
     (make-nodes
      ,(remove nil bindings) :prepend make-declaration-node)
     :body
     ,(when body
        ;; make single expression statements
        `(make-exprs ,body))))

(c-syntax function (name type parameters &body body &environment env)
  "Define c function"
  `(make-instance 'function-definition
    ;; function name + type
     :item
     ,(if (listp type)
         ;; check if macro/function or list
          (let ((first (first type)))
           (if (and (not (listp first)) (std:fboundp! first env))
               ;; type is macro or function
               `(make-declaration-node (,type ,name))
               ;; type is list with type information
               `(make-declaration-node (,@type ,name))))
         ;; type is single symbol
         `(make-declaration-node (,name ,type)))
     :parameter
     (make-instance 'parameter-list
       :parameters
       (make-nodes ,parameters :prepend make-declaration-node))
     :body
     ,(when body
        `(make-block ,body))))

(c-syntax fpointer (name &optional parameters)
  "Define a function pointer"
  `(make-instance 'function-pointer
    ;; function pointer identifier
     :identifier (make-node ,name)
    ;; function pointer parameters
     :parameters (make-instance 'parameter-list
                   :parameters (make-nodes ,parameters :prepend make-declaration-node))))

(c-syntax for (init &body body)
  "The c for loop"
  `(make-instance 'for-statement
    ;; check if initialization present
     :init
     ,(when (first init)
        ;; set init
        `(make-declaration-node ,(first init)))
    :test 
    (make-node ,(second init))
    :step
    (make-node ,(third init))
    :body
    (make-block ,body)))

(c-syntax if (test if-body &optional else-body)
  "The c if expression"
  `(make-instance 'if-statement
    ;; case test
     :test (make-node ,test)
    ;; if true:
     ;; TEST 2024-12-14: 
     :if-body (make-simple-block ,(when if-body if-body))
    ;; if else and present
     :else-body ,(when else-body
                   `(make-simple-block ,else-body))))

(c-syntax ? (test then else)
  "The conditinal expression 'test ? then : else'"
  `(make-instance 'conditional-expression
     :test (make-node ,test)
     :then (make-node ,then)
     :else (make-node ,else)))

(defmacro make-switch-case-item (item)
  "switch case item helper"
  `(make-instance 'switch-case-item
     :constant
     ;; list of trigger values
     ,(if (eql (first item) t)
          ;; identify default case
          nil 
          ;; normal cases
          `(make-nodes ,(if (listp (first item))
                            (first item)
                            (list (first item)))
                       :quoty t))
     :body
     (make-exprs ,(rest item))))

(c-syntax switch (expression &rest cases)
  "Switch-Case"
  `(make-instance 'switch-case-statement
    ;; set expression
     :switch
     (make-node ,expression)
     :cases
     (make-instance 'compound-statement
       :braces t
       :statements (make-nodes ,cases :prepend make-switch-case-item))))

(c-syntax while (test &body body)
  "The c while loop"
  `(make-instance 'while-statement
     :test
    (make-node ,test)
    :body
    (make-block ,body)))

(c-syntax do-while (test &body body)
   "The c do-whiel loop"
   `(make-instance 'do-statement
      :body
     (make-block ,body)
     :test
     (make-node ,test)))

(c-syntax typedef (&rest rest)
  "Typedef for c types"
  `(make-instance 'typedef
     :declaration
    (make-declaration-node ,rest)))

(c-syntax cast (&rest rest)
  "Cast type"
  `(make-instance 'cast-expression
     :type
    (decompose-type ,(butlast rest))
    :object 
    (make-node ,(first (last rest)))))

(c-syntax sizeof (&rest type)
  "C sizeof function"
  `(make-instance 'function-call
     :function
     (make-node sizeof)
     :arguments
    (decompose-type ,type)))

(c-syntax float-type (item)
  "Generate 'f' suffixes"
  `(make-instance 'float-type :number (make-node ,item)))

(c-syntax (goto continue break return) (&optional item)
  "Jump statements with optional item"
  `(make-instance 'jump-statement
     :kind
     (make-node ,syn/gen::tag)
     :members
     ,(when item `(make-node ,item))))

(c-syntax (label) (name)
  "Label"
  `(make-instance 'label-statement :name (make-node ,name)))

(c-syntax not (item)
  "Not-expression"
  `(make-instance 'not-expression :value (make-node ,item)))

(c-syntax clist (&rest rest)
  "C style list"
  `(make-instance 'clist :items (make-nodes ,rest)))

(c-syntax funcall (function &rest args)
  "C function call"
  `(make-instance 'function-call
     :function (make-node ,function)
     :arguments (make-nodes ,args)))

(c-syntax attribute (&rest args)
  "GCC attribute extension"
  `(make-instance 'attribute-expression
     :arguments (make-nodes ,args)))

(build-context-switches
 :package :syn/gen/c/sym
 :symbols *c-symbols*)

(build-swap-package
 :package :syn/gen/c/sym
 :swap-package :syn/gen/c/swap
 :symbols *c-swap*)

;;; Traversal
;;; A traverser which checks the identifier for c-conformity
;;; and automatically solves naming problems.
(defclass renamer ()
  ((used-names :initform (make-hash-table :test 'equal))
   (name-map :initform (make-hash-table :test 'equal))))
(defgeneric check-and-get-name (renamer check-name))

;;; Check if identifier is OK.
;;; Store in hash table and correct if necessary.
(defmethod check-and-get-name ((item renamer) check-name)
  (with-slots (used-names name-map) item
    (if (eql check-name '|...|)
        ;; ignore '...'
        check-name
        ;; treat hyphen and underscore equally / map hyphen to underscore
        (let* ((name-string (symbol-name check-name))
               (identifier (substitute #\_ #\- name-string)))
          (when (and (not (equal identifier name-string))
                     (find :hyphen *gen-warnings*))
            (warn "Possible ambiguity through hyphen override of ~s" check-name))
          (let ((alr-checked (gethash identifier name-map)))
           (if alr-checked
               alr-checked
               (labels ((check-char (x) (alpha-char-p x))
                        (check-underscore (x) (eql #\_ x))
                        (check-tilde (x) (eql #\~ x))
                        (check-num (x) (digit-char-p x))
                        (check-hex (x) (and (eql #\0 (first x))
                                            (or (eql #\x (second x))
                                                (eql #\X (second x)))))
                        (check-all (x)
                         (or
                           (check-char x)
                           (check-underscore x)
                           (check-num x)))
                        (check-nall (x)
                          (not (check-all x))))
                 (let* ((identifier-l (concatenate 'list identifier))
                        (changed-l (if (check-tilde (car identifier-l))
                                       (concatenate 'list
                                         '(#\~)
                                         (substitute-if #\_ #'check-nall (rest identifier-l)))
                                       (substitute-if #\_ #'check-nall identifier-l)))
                        (changed (concatenate 'string changed-l)))

                   (when (and (check-num (first changed-l))
                              (not (check-hex changed-l)))
                     (setf (first changed-l) #\_)
                     (setf changed (concatenate 'string changed-l)))

                   (loop while (gethash changed used-names) do
                     (setf changed (format nil "_~a" changed)))
                   (setf (gethash changed used-names) t)
                   (setf changed (intern changed))
                   (setf (gethash identifier name-map) changed)
                   changed))))))))

;;; Traverses the tree but checks only the identifier nodes.
(defmethod traverse ((rn renamer) (item ident) level)
  (declare (ignore level))
  (setf (val item)
        (check-and-get-name rn (val item))))

;;; This Traverser checks whether braces really are necessary.
(defclass decl-blocker ()
  ((names :initform `(,(make-hash-table)))
   (delta-names :initform '(nil))
   (in-decl :initform '(nil))
   (in-decl-item :initform '(nil))
   (make-block :initform '(nil))))

(defmethod traverse ((db decl-blocker) (item ident) level)
  "find names, check if in decl-item, save infos on stack in decl-blocker"
  (declare (ignore level))
  (with-slots (val) item
    (with-slots (names delta-names in-decl-item make-block) db
      (when (first in-decl-item)
        (if (gethash val (first names))
            (setf (first make-block) t)
            (progn (push val (first delta-names))
                   (setf (gethash val (first names)) t)))))))

(defmethod traverse :before ((db decl-blocker) (item declaration-list) level)
  "prepare empty lists and a nil-value for further traversing"
  (declare (ignore level))
  (with-slots (delta-names make-block in-decl in-decl-item) db
    (push nil delta-names)
    (push nil make-block)
    (push t in-decl)
    (push nil in-decl-item)))

(defmethod traverse :after ((db decl-blocker) (item declaration-list) level)
  "check values in decl-blocker and set braces to 'true' or 'nil'"
  (declare (ignore level))
  (with-slots (names delta-names make-block in-decl in-decl-item) db
    (if (first make-block)
        (progn
          (setf (slot-value item 'braces) t)
          (loop for i in (first delta-names) do
               (setf (gethash i (first names)) nil)))
        (if (> (list-length delta-names) 1)
            (progn
              (setf (slot-value item 'braces) nil)
              (loop for i in (first  delta-names) do
                (push i (second delta-names))))))
    (pop delta-names)
    (pop make-block)
    (pop in-decl)
    (pop in-decl-item)))

(defmacro prepare-blocker-stacks (node-class)
  "create method which prepares decl-blocker stacks"
  `(defmethod traverse :before ((db decl-blocker) (item ,node-class) level)
     "prepare empty decl-blocker stacks and values"
     (declare (ignore level))
     (with-slots (names) db
       (push (make-hash-table) names))))

(defmacro clean-blocker-stacks (node-class)
  "creates method which cleans decl-blocker stacks"
  `(defmethod traverse :after ((db decl-blocker) (item ,node-class) level)
     "clean up decl-blocker stack and values"
     (declare (ignore level))
     (with-slots (names) db
       (pop names))))

(defmacro decl-blocker-extra-nodes (&rest nodes)
  `(progn .,(loop for i in nodes collect
              `(progn (eval (prepare-blocker-stacks ,i))
                      (eval (clean-blocker-stacks ,i))))))

(decl-blocker-extra-nodes function-definition struct-definition for-statement compound-statement)

;;; This traverser hides "{}" in ifs where possible
(defclass if-blocker ()
  ((parent-node :initform '())
   (statement-count :initform '(0))
   (first-statement :initform '(nil))
   (self-else :initform '(nil))
   (child-else :initform '(nil))
   (force-braces :initform '(nil))
   (curr-level :initform '())))

(defmethod traverse :before ((ib if-blocker) (item compound-statement) level)
  "prepare stacks, count statements"
  (with-slots (parent-node statement-count first-statement force-braces curr-level) ib
    (with-slots (statements) item
      (push level curr-level)
      (push t first-statement)
      (push 'compound-statement parent-node)
      (push nil force-braces)
      (push 0 statement-count))))

(defmethod traverse :after ((ib if-blocker) (item compound-statement) level)
  "decide wheter to print braces or not"
  (with-slots (parent-node statement-count first-statement
               self-else child-else force-braces curr-level) 
      ib
    (with-slots (statement braces) item
      (pop parent-node)
      (pop curr-level)

      (cond ((eql (first parent-node) 'if-body)

             (cond ((and (< (first statement-count) 2)
                         (not (first self-else)))
                    (setf braces nil))
                   ((and (< (first statement-count) 2)
                         (first self-else)
                         (first child-else))
                    (setf braces nil))))

            ((eql (first parent-node) 'else-body)
             (if (< (first statement-count) 2)
                 (setf braces nil))))

      (if (first force-braces)
          (setf braces t))
      (pop statement-count)
      (pop first-statement)
      (pop force-braces))))

(defmethod traverse :after ((ib if-blocker) (item comment) level)
  "force braces if comments are present / important for solitary comments"
  (declare (ignore level))
  (with-slots (force-braces) ib
    (setf (first force-braces) t)))

(defmethod traverse :before ((ib if-blocker) (item declaration-list) level)
  "set force-braces (to t) if declartion-list found"
  (declare (ignore level))
  (with-slots (force-braces) ib
    (if force-braces
        (setf (first force-braces) t))))

(defmethod traverse :before ((ib if-blocker) (item ast) level)
  "check nodelists that belong to a compound-statement"
  (with-slots (statement-count first-statement parent-node curr-level) ib
    (with-slots (ast) item
      (when (and (first first-statement)
                 (eql (first parent-node) 'compound-statement) 
                 (eql (- level (first curr-level)) 2))

        (let ((count (length ast)))
          (setf (first first-statement) nil)
          (setf (first statement-count)
                (max count (first statement-count))))))))


(defmethod traverse :after ((ib if-blocker) (item expression-statement) level)
  "place semicolon at empty branches"
  (with-slots (statement-count curr-level force-braces) ib
    (with-slots (force-semicolon expression) item
      (if (and
           ;; subnode that can contain no further statements
           (typep expression 'ast)
           ;; do nothing if a comment is present, see above (if-blocker comment)
           (not (eql (first force-braces) t))
           ;; specific position in ast, 1st expr-statement in body.
           (and (first curr-level) ;; curr-level must be set
                (eql (- level (first curr-level)) 1))
           ;; subtree has no expressions
           ;; this is a :after method, statement-cound already filled
           (eql (first statement-count) 0))
          (setf force-semicolon t)))))

;;; This traveser removes ambiguous nested compound-statements in else-if
;;; to reduce indentation.
(defclass else-if-traverser ()())

;;; Remove nested ast (progn (progn (progn ...)))
;;; Required for proper placement of curly braces (esp. for if-else)
(defclass nested-ast-remover () ())
(defmethod traverse :after ((nar nested-ast-remover) (item ast) level)
  (with-slots (ast) item
    (when (and (eql (length ast) 1)
               (typep (first ast) 'expression-statement)
               (typep (slot-value (first ast) 'expression) 'ast))
      (setf ast (slot-value (slot-value (first ast) 'expression) 'ast)))))
