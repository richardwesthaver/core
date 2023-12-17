;;; fu.lisp --- Function utilities

;;; Code:
(in-package :std)

(in-readtable :std)

(defmacro! sortf (comparator &rest places)
  (if places
    `(tagbody
       ,@(mapcar
           #`(let ((,g!a #1=,(nth (car a1) places))
                   (,g!b #2=,(nth (cadr a1) places)))
               (if (,comparator ,g!b ,g!a)
                 (setf #1# ,g!b
                       #2# ,g!a)))
           (build-batcher-sn (length places))))))

#+cl-ppcre
(defun dollar-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 1)
       (string= (symbol-name s)
                "$"
                :start1 0
                :end1 1)
       (ignore-errors (parse-integer (subseq (symbol-name s) 1)))))


#+cl-ppcre
(defmacro! if-match ((match-regex str) then &optional else)
  (let* ((dollars (remove-duplicates
                   (remove-if-not #'dollar-symbol-p
                                  (flatten then))))
         (top (or (car (sort (mapcar #'dollar-symbol-p dollars) #'>))
                  0)))
    `(multiple-value-bind (,g!matches ,g!captures) (,match-regex ,str)
       (declare (ignorable ,g!matches ,g!captures))
       (let ((,g!captures-len (length ,g!captures)))
         (declare (ignorable ,g!captures-len))
         (symbol-macrolet ,(mapcar #`(,(symb "$" a1)
                                       (if (< ,g!captures-len ,a1)
                                           (error "Too few matchs: ~a unbound." ,(mkstr "$" a1))
                                           (aref ,g!captures ,(1- a1))))
                                   (loop for i from 1 to top collect i))
           (if ,g!matches
               ,then
               ,else))))))

#+cl-ppcre
(defmacro when-match ((match-regex str) &body forms)
  `(if-match (,match-regex ,str)
     (progn ,@forms)))

(defmacro once-only (specs &body forms)
  "Constructs code whose primary goal is to help automate the handling of
multiple evaluation within macros. Multiple evaluation is handled by introducing
intermediate variables, in order to reuse the result of an expression.

The returned value is a list of the form

  (let ((<gensym-1> <expr-1>)
        ...
        (<gensym-n> <expr-n>))
    <res>)

where GENSYM-1, ..., GENSYM-N are the intermediate variables introduced in order
to evaluate EXPR-1, ..., EXPR-N once, only. RES is code that is the result of
evaluating the implicit progn FORMS within a special context determined by
SPECS. RES should make use of (reference) the intermediate variables.

Each element within SPECS is either a symbol SYMBOL or a pair (SYMBOL INITFORM).
Bare symbols are equivalent to the pair (SYMBOL SYMBOL).

Each pair (SYMBOL INITFORM) specifies a single intermediate variable:

- INITFORM is an expression evaluated to produce EXPR-i

- SYMBOL is the name of the variable that will be bound around FORMS to the
  corresponding gensym GENSYM-i, in order for FORMS to generate RES that
  references the intermediate variable

The evaluation of INITFORMs and binding of SYMBOLs resembles LET. INITFORMs of
all the pairs are evaluated before binding SYMBOLs and evaluating FORMS.

Example:

  The following expression

  (let ((x '(incf y)))
    (once-only (x)
      `(cons ,x ,x)))

  ;;; =>
  ;;; (let ((#1=#:X123 (incf y)))
  ;;;   (cons #1# #1#))

  could be used within a macro to avoid multiple evaluation like so

  (defmacro cons1 (x)
    (once-only (x)
      `(cons ,x ,x)))

  (let ((y 0))
    (cons1 (incf y)))

  ;;; => (1 . 1)

Example:

  The following expression demonstrates the usage of the INITFORM field

  (let ((expr '(incf y)))
    (once-only ((var `(1+ ,expr)))
      `(list ',expr ,var ,var)))

  ;;; =>
  ;;; (let ((#1=#:VAR123 (1+ (incf y))))
  ;;;   (list '(incf y) #1# #1))

  which could be used like so

  (defmacro print-succ-twice (expr)
    (once-only ((var `(1+ ,expr)))
      `(format t \"Expr: ~s, Once: ~s, Twice: ~s~%\" ',expr ,var ,var)))

  (let ((y 10))
    (print-succ-twice (incf y)))

  ;;; >>
  ;;; Expr: (INCF Y), Once: 12, Twice: 12"
  (let ((gensyms (make-gensym-list (length specs) "ONCE-ONLY"))
        (names-and-forms (mapcar (lambda (spec)
                                   (etypecase spec
                                     (list
                                      (destructuring-bind (name form) spec
                                        (cons name form)))
                                     (symbol
                                      (cons spec spec))))
                                 specs)))
    ;; bind in user-macro
    `(let ,(mapcar (lambda (g n) (list g `(gensym ,(string (car n)))))
                   gensyms names-and-forms)
       ;; bind in final expansion
       `(let (,,@(mapcar (lambda (g n)
                           ``(,,g ,,(cdr n)))
                         gensyms names-and-forms))
          ;; bind in user-macro
          ,(let ,(mapcar (lambda (n g) (list (car n) g))
                         names-and-forms gensyms)
             ,@forms)))))

;;;; DESTRUCTURING-*CASE

(defun expand-destructuring-case (key clauses case)
  (once-only (key)
    `(if (typep ,key 'cons)
         (,case (car ,key)
           ,@(mapcar (lambda (clause)
                       (destructuring-bind ((keys . lambda-list) &body body) clause
                         `(,keys
                           (destructuring-bind ,lambda-list (cdr ,key)
                             ,@body))))
                     clauses))
         (error "Invalid key to DESTRUCTURING-~S: ~S" ',case ,key))))

(defmacro destructuring-case (keyform &body clauses)
  "DESTRUCTURING-CASE, -CCASE, and -ECASE are a combination of CASE and DESTRUCTURING-BIND.
KEYFORM must evaluate to a CONS.

Clauses are of the form:

  ((CASE-KEYS . DESTRUCTURING-LAMBDA-LIST) FORM*)

The clause whose CASE-KEYS matches CAR of KEY, as if by CASE, CCASE, or ECASE,
is selected, and FORMs are then executed with CDR of KEY is destructured and
bound by the DESTRUCTURING-LAMBDA-LIST.

Example:

 (defun dcase (x)
   (destructuring-case x
     ((:foo a b)
      (format nil \"foo: ~S, ~S\" a b))
     ((:bar &key a b)
      (format nil \"bar: ~S, ~S\" a b))
     (((:alt1 :alt2) a)
      (format nil \"alt: ~S\" a))
     ((t &rest rest)
      (format nil \"unknown: ~S\" rest))))

  (dcase (list :foo 1 2))        ; => \"foo: 1, 2\"
  (dcase (list :bar :a 1 :b 2))  ; => \"bar: 1, 2\"
  (dcase (list :alt1 1))         ; => \"alt: 1\"
  (dcase (list :alt2 2))         ; => \"alt: 2\"
  (dcase (list :quux 1 2 3))     ; => \"unknown: 1, 2, 3\"

 (defun decase (x)
   (destructuring-case x
     ((:foo a b)
      (format nil \"foo: ~S, ~S\" a b))
     ((:bar &key a b)
      (format nil \"bar: ~S, ~S\" a b))
     (((:alt1 :alt2) a)
      (format nil \"alt: ~S\" a))))

  (decase (list :foo 1 2))        ; => \"foo: 1, 2\"
  (decase (list :bar :a 1 :b 2))  ; => \"bar: 1, 2\"
  (decase (list :alt1 1))         ; => \"alt: 1\"
  (decase (list :alt2 2))         ; => \"alt: 2\"
  (decase (list :quux 1 2 3))     ; =| error
"
  (expand-destructuring-case keyform clauses 'case))

(defmacro destructuring-ccase (keyform &body clauses)
  (expand-destructuring-case keyform clauses 'ccase))

(defmacro destructuring-ecase (keyform &body clauses)
  (expand-destructuring-case keyform clauses 'ecase))

(dolist (name '(destructuring-ccase destructuring-ecase))
  (setf (documentation name 'function) (documentation 'destructuring-case 'function)))

;;; *-let --- control-flow let-binding macros
;; based on https://stevelosh.com/blog/2018/07/fun-with-macros-if-let/

(defmacro when-let (bindings &body body)
  "Bind `bindings` in parallel and execute `body`, short-circuiting on `nil`.

  This macro combines `when` and `let`.  It takes a list of bindings and
  binds them like `let` before executing `body`, but if any binding's value
  evaluates to `nil` the process stops and `nil` is immediately returned.

  Examples:

    (when-let ((a (progn (print :a) 1))
               (b (progn (print :b) 2))
      (list a b))
    ; =>
    :A
    :B
    (1 2)

    (when-let ((a (progn (print :a) nil))
               (b (progn (print :b) 2)))
      (list a b))
    ; =>
    :A
    NIL

  "
  (with-gensyms (block)
    `(block ,block
       (let ,(loop :for (symbol value) :in bindings
                   :collect `(,symbol (or ,value
                                          (return-from ,block nil))))
         ,@body))))

(defmacro when-let* (bindings &body body)
  "Bind `bindings` serially and execute `body`, short-circuiting on `nil`.

  This macro combines `when` and `let*`.  It takes a list of bindings
  and binds them like `let*` before executing `body`, but if any
  binding's value evaluates to `nil` the process stops and `nil` is
  immediately returned.

  Examples:

    (when-let* ((a (progn (print :a) 1))
                (b (progn (print :b) (1+ a)))
      (list a b))
    ; =>
    :A
    :B
    (1 2)

    (when-let* ((a (progn (print :a) nil))
                (b (progn (print :b) (1+ a))))
      (list a b))
    ; =>
    :A
    NIL

  "
  (with-gensyms (block)
    `(block ,block
       (let* ,(loop :for (symbol value) :in bindings
                    :collect `(,symbol (or ,value
                                           (return-from ,block nil))))
         ,@body))))

(defmacro if-let (bindings &body body)
  "Bind `bindings` in parallel and execute `then` if all are true, or `else` otherwise.

  `body` must be of the form `(...optional-declarations... then else)`.

  This macro combines `if` and `let`.  It takes a list of bindings and
  binds them like `let` before executing the `then` branch of `body`, but
  if any binding's value evaluates to `nil` the process stops there and the
  `else` branch is immediately executed (with no bindings in effect).

  If any `optional-declarations` are included they will only be in effect
  for the `then` branch.

  Examples:

    (if-let ((a (progn (print :a) 1))
             (b (progn (print :b) 2)))
      (list a b)
      'nope)
    ; =>
    :A
    :B
    (1 2)

    (if-let ((a (progn (print :a) nil))
             (b (progn (print :b) 2)))
      (list a b)
      'nope)
    ; =>
    :A
    NOPE

  "
  (with-gensyms (outer inner)
    (multiple-value-bind (body declarations) (parse-body body)
      (destructuring-bind (then else) body
        `(block ,outer
           (block ,inner
             (let ,(loop :for (symbol value) :in bindings
                         :collect `(,symbol (or ,value
                                                (return-from ,inner nil))))
               ,@declarations
               (return-from ,outer ,then)))
           ,else)))))

(defmacro if-let* (bindings then else)
  "Bind `bindings` serially and execute `then` if all are true, or `else` otherwise.

  This macro combines `if` and `let*`.  It takes a list of bindings and
  binds them like `let*` before executing `then`, but if any binding's
  value evaluates to `nil` the process stops and the `else` branch is
  immediately executed (with no bindings in effect).

  Examples:

    (if-let* ((a (progn (print :a) 1))
              (b (progn (print :b) (1+ a)))
      (list a b)
      'nope)
    ; =>
    :A
    :B
    (1 2)

    (if-let* ((a (progn (print :a) nil))
              (b (progn (print :b) (1+ a))))
      (list a b)
      'nope)
    ; =>
    :A
    NOPE

  "
  (with-gensyms (outer inner)
    `(block ,outer
       (block ,inner
         (let* ,(loop :for (symbol value) :in bindings
                      :collect `(,symbol (or ,value
                                             (return-from ,inner nil))))
           (return-from ,outer ,then)))
       ,else)))


(defmacro def! (name &body body)
  "`defun' without args."
  `(defun ,name () ,@body))

(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute) ,@body))

;;; TODO 2023-09-04: Env

;;; Introspection
;; (eval-always (require :sb-introspect))

;; (reexport-from :sb-introspect
;; 	       :include '(:function-lambda-list :lambda-list-keywords :lambda-parameters-limit
;; 			  :method-combination-lambda-list :deftype-lambda-list
;; 			  :primitive-object-size :allocation-information
;; 			  :function-type
;; 			  :who-specializes-directly :who-specializes-generally
;; 			  :find-function-callees :find-function-callers))

;; ;;; Compiler

;; (reexport-from :sb-c
;; 	       :include '(:define-source-transformation
;; 			  :parse-eval-when-situations
;; 			  :source-location))
;;; Definitions
(defun %reevaluate-constant (name value test)
  (if (not (boundp name))
      value
      (let ((old (symbol-value name))
            (new value))
        (if (not (constantp name))
            (prog1 new
              (cerror "Try to redefine the variable as a constant."
                      "~@<~S is an already bound non-constant variable ~
                       whose value is ~S.~:@>" name old))
            (if (funcall test old new)
                old
                (restart-case
                    (error "~@<~S is an already defined constant whose value ~
                              ~S is not equal to the provided initial value ~S ~
                              under ~S.~:@>" name old new test)
                  (ignore ()
                    :report "Retain the current value."
                    old)
                  (continue ()
                    :report "Try to redefine the constant."
                    new)))))))

(defmacro define-constant (name initial-value &key (test ''eql) documentation)
  "Ensures that the global variable named by NAME is a constant with a value
that is equal under TEST to the result of evaluating INITIAL-VALUE. TEST is a
/function designator/ that defaults to EQL. If DOCUMENTATION is given, it
becomes the documentation string of the constant.

Signals an error if NAME is already a bound non-constant variable.

Signals an error if NAME is already a constant variable whose value is not
equal under TEST to result of evaluating INITIAL-VALUE."
  `(defconstant ,name (%reevaluate-constant ',name ,initial-value ,test)
     ,@(when documentation `(,documentation))))

;;; Named Lambdas
;; (reexport-from :sb-int :include '(:make-macro-lambda :parse-lambda-list))

;;; Sexp utils
;; (reexport-from :uiop :include '(read-file-form read-file-forms slurp-stream-forms))

;;; cl-bench utils
;; Destructive merge of two sorted lists.
;; From Hansen's MS thesis.
(defun merge! (a b predicate)
  (labels ((merge-loop (r a b)
             (cond ((funcall predicate (car b) (car a))
                    (setf (cdr r) b)
                    (if (null (cdr b))
                        (setf (cdr b) a)
                        (merge-loop b a (cdr b))))
                   (t ; (car a) <= (car b)
                    (setf (cdr r) a)
                    (if (null (cdr a))
                        (setf (cdr a) b)
                        (merge-loop a (cdr a) b))))))
    (cond ((null a) b)
          ((null b) a)
          ((funcall predicate (car b) (car a))
           (if (null (cdr b))
               (setf (cdr b) a)
               (merge-loop b a (cdr b)))
           b)
          (t                           ; (car a) <= (car b)
           (if (null (cdr a))
               (setf (cdr a) b)
               (merge-loop a (cdr a) b))
           a))))

;; Stable sort procedure which copies the input list and then sorts
;; the new list imperatively.  On the systems we have benchmarked,
;; this generic list sort has been at least as fast and usually much
;; faster than the library's sort routine.
;; Due to Richard O'Keefe; algorithm attributed to D.H.D. Warren.
(defun sort! (seq predicate)
  (labels ((astep (n)
             (cond ((> n 2)
                    (let* ((j (truncate n 2))
                           (a (astep j))
                           (k (- n j))
                           (b (astep k)))
                      (merge! a b predicate)))
                   ((= n 2)
                    (let ((x (car seq))
                          (y (cadr seq))
                          (p seq))
                      (setf seq (cddr seq))
                      (when (funcall predicate y x)
                        (setf (car p) y)
                        (setf (cadr p) x))
                      (setf (cddr p) nil)
                      p))
                   ((= n 1)
                    (let ((p seq))
                      (setf seq (cdr seq))
                      (setf (cdr p) nil)
                      p))
                   (t nil))))
    (astep (length seq))))

;;; CLOS/MOP
(defun list-indirect-class-methods (class)
  "List all indirect methods of CLASS."
  (remove-duplicates (mapcan #'specializer-direct-generic-functions (compute-class-precedence-list class))))

(defun list-class-methods (class methods &optional indirect)
  "List all methods specializing on CLASS modulo METHODS. When INDIRECT is
non-nil, also include indirect (parent) methods."
  (if (eq methods t)
      (if indirect
	  (list-indirect-class-methods class)
	  (specializer-direct-generic-functions class))
      (mapcar
       (lambda (s)
	 (car (member s (specializer-direct-generic-functions class) :key #'generic-function-name)))
       methods)))

;; FIX 2023-09-13: need exclude param
(defun list-class-slots (class slots &optional exclude)
  ;; should probably convert slot-definition-name here
  (let ((cs (remove-if
	     (lambda (s)
	       (or
		(null s)
		(member t (mapcar
			   (lambda (x)
			     (string= (slot-definition-name s) x))
			   exclude))))
	     (class-slots class))))
    (if (eq slots t)
	cs
	(loop for s in slots
	      with sn = (symb s)
	      for c in cs
	      with cn = (symb (slot-definition-name c))
	      when (eq sn cn)
		collect c))))

;; TODO 2023-09-09: slot exclusion from dynamic var
(defun list-slot-values-using-class (class obj slots &optional nullp unboundp)
  (remove-if
   #'null
   (mapcar
    (lambda (s)
      (let ((n (slot-definition-name s)))
	(let ((ns (make-keyword (symbol-name n))))
	  (if (slot-boundp-using-class class obj s)
	      (let ((v (slot-value-using-class class obj s)))
		(if nullp
		    `(,ns ,v)
		    (unless (null v)
		      `(,ns ,v))))
	      (when unboundp (list ns))))))
    slots)))

;;; Seq utils

(deftype signed-array-length ()
  "A (possibly negated) array length."
  '#.(let ((limit (1- array-dimension-limit)))
       `(integer ,(- limit) ,limit)))

(defun take (n seq)
  "Return, at most, the first N elements of SEQ, as a *new* sequence
of the same type as SEQ.

If N is longer than SEQ, SEQ is simply copied.

If N is negative, then |N| elements are taken (in their original
order) from the end of SEQ."
  #+sbcl (declare (sb-ext:muffle-conditions style-warning))
  (declare (type signed-array-length n))
  (seq-dispatch seq
    (if (minusp n)
        (last seq (abs n))
        (firstn n seq))
    (if (minusp n)
        (subseq seq (max 0 (+ (length seq) n)))
        (subseq seq 0 (min n (length seq))))))

;;; Hashtable utils
(declaim (inline maphash-keys))
(defun maphash-keys (function table)
  "Like MAPHASH, but calls FUNCTION with each key in the hash table TABLE."
  (maphash (lambda (k v)
             (declare (ignore v))
             (funcall function k))
           table))

(declaim (inline maphash-values))
(defun maphash-values (function table)
  "Like MAPHASH, but calls FUNCTION with each value in the hash table TABLE."
  (maphash (lambda (k v)
             (declare (ignore k))
             (funcall function v))
           table))

(defun hash-table-keys (table)
  "Returns a list containing the keys of hash table TABLE."
  (let ((keys nil))
    (maphash-keys (lambda (k)
                    (push k keys))
                  table)
    keys))

(defun hash-table-values (table)
  "Returns a list containing the values of hash table TABLE."
  (let ((values nil))
    (maphash-values (lambda (v)
                      (push v values))
                    table)
    values))

(defun my-lisp-implementation ()
  "Return the current lisp implemenation as a cons: (TYPE VERSION)"
  (list 
   (lisp-implementation-type) 
   (lisp-implementation-version)
   *features*))

;;; Franz
(defvar if*-keyword-list '("then" "thenret" "else" "elseif"))

(defmacro if* (&rest args)
   (do ((xx (reverse args) (cdr xx))
	(state :init)
	(elseseen nil)
	(totalcol nil)
	(lookat nil nil)
	(col nil))
       ((null xx)
	(cond ((eq state :compl)
	       `(cond ,@totalcol))
	      (t (error "if*: illegal form ~s" args))))
       (cond ((and (symbolp (car xx))
		   (member (symbol-name (car xx))
			   if*-keyword-list
			   :test #'string-equal))
	      (setq lookat (symbol-name (car xx)))))

       (cond ((eq state :init)
	      (cond (lookat (cond ((string-equal lookat "thenret")
				   (setq col nil
					 state :then))
				  (t (error
				      "if*: bad keyword ~a" lookat))))
		    (t (setq state :col
			     col nil)
		       (push (car xx) col))))
	     ((eq state :col)
	      (cond (lookat
		     (cond ((string-equal lookat "else")
			    (cond (elseseen
				   (error
				    "if*: multiples elses")))
			    (setq elseseen t)
			    (setq state :init)
			    (push `(t ,@col) totalcol))
			   ((string-equal lookat "then")
			    (setq state :then))
			   (t (error "if*: bad keyword ~s"
					      lookat))))
		    (t (push (car xx) col))))
	     ((eq state :then)
	      (cond (lookat
		     (error
		      "if*: keyword ~s at the wrong place " (car xx)))
		    (t (setq state :compl)
		       (push `(,(car xx) ,@col) totalcol))))
	     ((eq state :compl)
	      (cond ((not (string-equal lookat "elseif"))
		     (error "if*: missing elseif clause ")))
	      (setq state :init)))))
