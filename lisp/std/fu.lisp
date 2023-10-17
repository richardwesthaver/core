;;; fu.lisp --- Function utilities

;;; Code:
(defpackage :std/fu
  (:nicknames :fu)
  (:use :cl :sb-mop :sb-c :std/named-readtables :std/sym :std/list :std/cond)
  (:export
   :until
   #:mkstr
   #:symb
   #:group
   #:flatten
   #:fact
   #:choose
   #:g!-symbol-p
   #:defmacro/g!
   #:o!-symbol-p
   #:o!-symbol-to-g!-symbol
   #:defmacro!
   #:defun!
   #:|#"-reader|
   #:|#`-reader|
   #:|#f-reader|
   #:|#$-reader|
   #:segment-reader
   #:match-mode-ppcre-lambda-form
   #:subst-mode-ppcre-lambda-form
   #+cl-ppcre #:|#~-reader|
   #:dlambda
   #:make-tlist
   #:tlist-left
   #:tlist-right
   #:tlist-empty-p
   #:tlist-add-left
   #:tlist-add-right
   #:tlist-rem-left
   #:tlist-update
   #:build-batcher-sn
   #:sortf
   #:dollar-symbol-p
   #:if-match
   #:when-match
   #:once-only
   #:destructuring-case
   #:destructuring-ccase
   #:destructuring-ecase
   #:when-let
   #:when-let*
   #:if-let
   #:if-let*
   :define-constant
   :def!
   :eval-always
   :merge! :sort!
   :list-slot-values-using-class :list-class-methods :list-class-slots :list-indirect-slot-methods))

(in-package :std/fu)

;;; Misc
(defmacro until (condition &body body)
  (let ((block-name (gensym)))
    `(block ,block-name
       (loop
	 (if ,condition
	     (return-from ,block-name nil)
	     (progn ,@body))))))

;;; From LOL

(defun group (source n)
  (when (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons
                               (subseq source 0 n)
                               acc))
                   (nreverse
                     (cons source acc))))))
    (if source (rec source nil) nil)))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))

  (defun symb (&rest args)
    (values (intern (apply #'mkstr args))))

  (defun flatten (x)
    (labels ((rec (x acc)
                  (cond ((null x) acc)
                        #+sbcl
                        ((typep x 'sb-impl::comma) (rec (sb-impl::comma-expr x) acc))
                        ((atom x) (cons x acc))
                        (t (rec
                             (car x)
                             (rec (cdr x) acc))))))
      (rec x nil)))

  (defun g!-symbol-p (s)
    (and (symbolp s)
         (> (length (symbol-name s)) 2)
         (string= (symbol-name s)
                  "G!"
                  :start1 0
                  :end1 2)))

  (defun o!-symbol-p (s)
    (and (symbolp s)
         (> (length (symbol-name s)) 2)
         (string= (symbol-name s)
                  "O!"
                  :start1 0
                  :end1 2)))

  (defun o!-symbol-to-g!-symbol (s)
    (symb "G!"
          (subseq (symbol-name s) 2))))

(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p
                              (flatten body)))))
    (multiple-value-bind (body declarations docstring)
        (parse-body body :documentation t)
      `(defmacro ,name ,args
         ,@(when docstring
             (list docstring))
         ,@declarations
         (let ,(mapcar
                (lambda (s)
                  `(,s (gensym ,(subseq
                                 (symbol-name s)
                                 2))))
                syms)
           ,@body)))))

(defmacro defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p (flatten args)))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    (multiple-value-bind (body declarations docstring)
        (parse-body body :documentation t)
      `(defmacro/g! ,name ,args
         ,@(when docstring
            (list docstring))
         ,@declarations
         `(let ,(mapcar #'list (list ,@gs) (list ,@os))
            ,(progn ,@body))))))

(defmacro defun! (name args &body body)
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p
                              (flatten body)))))
    (multiple-value-bind (body declarations docstring)
        (parse-body body :documentation t)
      `(defun ,name ,args
         ,@(when docstring
             (list docstring))
         ,@declarations
         (let ,(mapcar (lambda (s)
                         `(,s (gensym ,(subseq (symbol-name s)
                                               2))))
                       syms)
           ,@body)))))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun |#`-reader| (stream sub-char numarg)
    (declare (ignore sub-char))
    (unless numarg (setq numarg 1))
    `(lambda ,(loop for i from 1 to numarg
                 collect (symb 'a i))
       ,(funcall
         (get-macro-character #\`) stream nil)))

  (defun |#f-reader| (stream sub-char numarg)
    (declare (ignore stream sub-char))
    (setq numarg (or numarg 3))
    (unless (<= numarg 3)
      (error "Bad value for #f: ~a" numarg))
    `(declare (optimize (speed ,numarg)
                        (safety ,(- 3 numarg)))))

  (defun |#$-reader| (stream sub-char numarg)
    "Switch on the shell reader, parsing STREAM and returning a
POSIX-compliant shell program as a string. In other words, this is an
implementation of the lazy version of SHCL's #$-reader.

Similar to shcl, we add some reader extensions to enable embedding
lisp forms and other goodies.

#$ x=,(* 2 2) 
echo $x
$#
;; => 4"
    (declare (ignore sub-char numarg))
    (let (chars (state 'sh))
      (loop do
	(let ((c (read-char stream)))
	  (cond 
	    ((eq state 'sh)
	     (when (char= c #\$) (setq state 'dolla))
	     (push c chars))
	    ((eq state 'dolla)
	     (cond
	       ((char= c #\#)
		;; remove trailing '$'
		(pop chars)
		(return))
	       (t (setq state 'sh) (push c chars)))))))
      (coerce (nreverse chars) 'string))))

;; Nestable suggestion from Daniel Herring
(eval-when (:compile-toplevel :load-toplevel :execute)
 (defun |#"-reader| (stream sub-char numarg)
   (declare (ignore sub-char numarg))
   (let (chars (state 'normal) (depth 1))
     (loop do
          (let ((curr (read-char stream)))
            (cond ((eq state 'normal)
                   (cond ((char= curr #\#)
                          (push #\# chars)
                          (setq state 'read-sharp))
                         ((char= curr #\")
                          (setq state 'read-quote))
                         (t
                          (push curr chars))))
                  ((eq state 'read-sharp)
                   (cond ((char= curr #\")
                          (push #\" chars)
                          (incf depth)
                          (setq state 'normal))
                         (t
                          (push curr chars)
                          (setq state 'normal))))
                  ((eq state 'read-quote)
                   (cond ((char= curr #\#)
                          (decf depth)
                          (if (zerop depth) (return))
                          (push #\" chars)
                          (push #\# chars)
                          (setq state 'normal))
                         (t
                          (push #\" chars)
                          (if (char= curr #\")
                              (setq state 'read-quote)
                              (progn
                                (push curr chars)
                                (setq state 'normal)))))))))
     (coerce (nreverse chars) 'string))))

; This version is from Martin Dirichs
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun |#>-reader| (stream sub-char numarg)
    (declare (ignore sub-char numarg))
    (let (chars)
      (do ((curr (read-char stream)
                 (read-char stream)))
          ((char= #\newline curr))
        (push curr chars))
      (let ((pattern (nreverse chars))
            output)
        (labels ((match (pos chars)
                   (if (null chars)
                       pos
                       (if (char= (nth pos pattern) (car chars))
                           (match (1+ pos) (cdr chars))
                           (match 0 (cdr (append (subseq pattern 0 pos) chars)))))))
          (do (curr
               (pos 0))
              ((= pos (length pattern)))
            (setf curr (read-char stream)
                  pos (match pos (list curr)))
            (push curr output))
          (coerce
           (nreverse
            (nthcdr (length pattern) output))
           'string))))))

; (set-dispatch-macro-character #\# #\> #'|#>-reader|)

(defun segment-reader (stream ch n)
  (if (> n 0)
    (let ((chars))
      (do ((curr (read-char stream)
                 (read-char stream)))
          ((char= ch curr))
        (push curr chars))
      (cons (coerce (nreverse chars) 'string)
            (segment-reader stream ch (- n 1))))))

#+cl-ppcre
(defmacro! match-mode-ppcre-lambda-form (o!args o!mods)
 ``(lambda (,',g!str)
     (cl-ppcre:scan-to-strings
       ,(if (zerop (length ,g!mods))
          (car ,g!args)
          (format nil "(?~a)~a" ,g!mods (car ,g!args)))
       ,',g!str)))

#+cl-ppcre
(defmacro! subst-mode-ppcre-lambda-form (o!args)
 ``(lambda (,',g!str)
     (cl-ppcre:regex-replace-all
       ,(car ,g!args)
       ,',g!str
       ,(cadr ,g!args))))

#+cl-ppcre
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun |#~-reader| (stream sub-char numarg)
    (declare (ignore sub-char numarg))
    (let ((mode-char (read-char stream)))
      (cond
        ((char= mode-char #\m)
         (match-mode-ppcre-lambda-form
          (segment-reader stream
                          (read-char stream)
                          1)
          (coerce (loop for c = (read-char stream)
                     while (alpha-char-p c)
                     collect c
                     finally (unread-char c stream))
                  'string)))
        ((char= mode-char #\s)
         (subst-mode-ppcre-lambda-form
          (segment-reader stream
                          (read-char stream)
                          2)))
        (t (error "Unknown #~~ mode character"))))))

#+cl-ppcre (set-dispatch-macro-character #\# #\~ #'|#~-reader|)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defreadtable :std
  (:merge :modern)
  (:dispatch-macro-char #\# #\" #'|#"-reader|)
  (:dispatch-macro-char #\# #\> #'|#>-reader|)
  #+cl-ppcre (:dispatch-macro-char #\# #\~ #'|#~-reader|)
  (:dispatch-macro-char #\# #\` #'|#`-reader|)
  (:dispatch-macro-char #\# #\f #'|#f-reader|)
  (:dispatch-macro-char #\# #\$ #'|#$-reader|)))

(defmacro! dlambda (&rest ds)
  "Dynamic dispatch lambda."
  `(lambda (&rest ,g!args)
     (case (car ,g!args)
       ,@(mapcar
           (lambda (d)
             `(,(if (eq t (car d))
                  t
                  (list (car d)))
               (apply (lambda ,@(cdr d))
                      ,(if (eq t (car d))
                         g!args
                         `(cdr ,g!args)))))
           ds))))

(declaim (inline make-tlist tlist-left
                 tlist-right tlist-empty-p))

(defun make-tlist () (cons nil nil))
(defun tlist-left (tl) (caar tl))
(defun tlist-right (tl) (cadr tl))
(defun tlist-empty-p (tl) (null (car tl)))

(declaim (inline tlist-add-left
                 tlist-add-right))

(defun tlist-add-left (tl it)
  (let ((x (cons it (car tl))))
    (if (tlist-empty-p tl)
      (setf (cdr tl) x))
    (setf (car tl) x)))

(defun tlist-add-right (tl it)
  (let ((x (cons it nil)))
    (if (tlist-empty-p tl)
      (setf (car tl) x)
      (setf (cddr tl) x))
    (setf (cdr tl) x)))

(declaim (inline tlist-rem-left))

(defun tlist-rem-left (tl)
  (if (tlist-empty-p tl)
    (error "Remove from empty tlist")
    (let ((x (car tl)))
      (setf (car tl) (cdar tl))
      (if (tlist-empty-p tl)
        (setf (cdr tl) nil)) ;; For gc
      (car x))))

(declaim (inline tlist-update))

(defun tlist-update (tl)
  (setf (cdr tl) (last (car tl))))

(defun build-batcher-sn (n)
  (let* (network
         (tee (ceiling (log n 2)))
         (p (ash 1 (- tee 1))))
    (loop while (> p 0) do
      (let ((q (ash 1 (- tee 1)))
            (r 0)
            (d p))
        (loop while (> d 0) do
          (loop for i from 0 to (- n d 1) do
            (if (= (logand i p) r)
              (push (list i (+ i d))
                    network)))
          (setf d (- q p)
                q (ash q -1)
                r p)))
      (setf p (ash p -1)))
    (nreverse network)))

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
