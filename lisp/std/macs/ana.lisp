;;; ana.lisp --- anaphoric macros

;;; Code:
(in-package :std/macs)

(in-readtable :std)

;;; Named Lambdas
;; (reexport-from :sb-int :include '(:make-macro-lambda :parse-lambda-list))

;; LoL tlist
;; (declaim (inline make-tlist tlist-left
;;                  tlist-right tlist-empty-p))

;; (defun make-tlist () (cons nil nil))
;; (defun tlist-left (tl) (caar tl))
;; (defun tlist-right (tl) (cadr tl))
;; (defun tlist-empty-p (tl) (null (car tl)))

;; (declaim (inline tlist-add-left
;;                  tlist-add-right))

;; (defun tlist-add-left (tl it)
;;   (let ((x (cons it (car tl))))
;;     (if (tlist-empty-p tl)
;;         (setf (cdr tl) x))
;;     (setf (car tl) x)))

;; (defun tlist-add-right (tl it)
;;   (let ((x (cons it nil)))
;;     (if (tlist-empty-p tl)
;;         (setf (car tl) x)
;;         (setf (cddr tl) x))
;;     (setf (cdr tl) x)))

;; (declaim (inline tlist-rem-left))

;; (defun tlist-rem-left (tl)
;;   (if (tlist-empty-p tl)
;;       (error "Remove from empty tlist")
;;       (let ((x (car tl)))
;;         (setf (car tl) (cdar tl))
;;         (if (tlist-empty-p tl)
;;             (setf (cdr tl) nil)) ;; For gc
;;         (car x))))

;; (declaim (inline tlist-update))

;; (defun tlist-update (tl)
;;   (setf (cdr tl) (last (car tl))))

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

(defmacro named-lambda (name lambda-list &body body)
  "Expands into a lambda-expression within whose BODY NAME denotes the
corresponding function."
  `(labels ((,name ,lambda-list ,@body))
     #',name))

;;; Misc
(defmacro until (condition &body body)
  (let ((block-name (gensym)))
    `(block ,block-name
       (loop
         (if ,condition
             (return-from ,block-name nil)
             (progn ,@body))))))

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

;; Graham's alambda
(defmacro alambda (parms &body body)
  `(labels ((%a ,parms ,@body))
     #'%a))

;; Graham's aif
(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

;; ;; TODO 2023-09-05: wrap, document, optimize, hack
;; re-exported from SB-INT
(defmacro awhen (test &body body)
  `(let ((it ,test))
     (when it ,@body)))

(defmacro acond (&rest clauses)
  (if (null clauses)
      `()
      (destructuring-bind ((test &body body) &rest rest) clauses
        (let ((it (copy-symbol 'it)))
          `(let ((,it ,test))
             (if ,it
                 ;; Just like COND - no body means return the tested value.
                 ,(if body
                      `(let ((it ,it)) (declare (ignorable it)) ,@body)
                      it)
                 (acond ,@rest)))))))

(defmacro! nlet-tail (n letargs &body body)
  (let ((gs (loop for i in letargs
                  collect (gensym))))
    `(macrolet
         ((,n ,gs
            `(progn
               (psetq
                ,@(apply #'nconc
                         (mapcar
                          #'list
                          ',(mapcar #'car letargs)
                          (list ,@gs))))
               (go ,',g!n))))
       (block ,g!b
         (let ,letargs
           (tagbody
              ,g!n (return-from
                    ,g!b (progn ,@body))))))))

(defmacro alet% (letargs &rest body)
  `(let ((%a) ,@letargs)
     (setq %a ,@(last body))
     ,@(butlast body)
     %a))

(defmacro alet (letargs &rest body)
  `(let ((%a) ,@letargs)
     (setq %a ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply %a params))))

;; swiped from fiveam. This is just like acond except it assumes that
;; the TEST in each element of CLAUSES returns two values as opposed
;; to one.
(defmacro acond2 (&rest clauses)
  (if (null clauses)
      nil
      (with-gensyms (val foundp)
        (destructuring-bind ((test &rest progn) &rest others)
            clauses
          `(multiple-value-bind (,val ,foundp)
               ,test
             (if (or ,val ,foundp)
                 (let ((it ,val))
                   (declare (ignorable it))
                   ,@progn)
                 (acond2 ,@others)))))))
