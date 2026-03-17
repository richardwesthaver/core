;;; std/macs/control.lisp --- Control Flow Macros

;;

;;; Code:
(in-package :std/macs)

;; ported from CL-UTILITIES

;; This should only be called inside of COLLECTING macros, but we
;; define it here to provide an informative error message and to make
;; it easier for SLIME (et al.) to get documentation for the COLLECT
;; function when it's used in the COLLECTING macro.
(defun collect (thing)
  "Collect THING in the context established by the COLLECTING macro"
  (error "Can't collect ~S outside the context of the COLLECTING macro"
         thing))

(defmacro collecting (&body body)
  "Collect things into a list forwards. Within the body of this macro,
the COLLECT function will collect its argument into the list returned
by COLLECTING."
  (with-gensyms (collector tail)
    `(let (,collector ,tail)
      (labels ((collect (thing)
                 (if ,collector
                     (setf (cdr ,tail)
                           (setf ,tail (list thing)))
                     (setf ,collector
                           (setf ,tail (list thing))))))
        ,@body)
      ,collector)))

(defmacro with-collectors ((&rest collectors) &body body)
  "Collect some things into lists forwards. The names in COLLECTORS
are defined as local functions which each collect into a separate
list.  Returns as many values as there are collectors, in the order
they were given."
  (%with-collectors-check-collectors collectors)
  (let ((gensyms-alist (%with-collectors-gensyms-alist collectors)))
    `(let ,(loop for collector in collectors
                 for tail = (cdr (assoc collector gensyms-alist))
                 nconc (list collector tail))
      (labels ,(loop for collector in collectors
                     for tail = (cdr (assoc collector gensyms-alist))
                     collect `(,collector (thing)
                               (if ,collector
                                   (setf (cdr ,tail)
                                         (setf ,tail (list thing)))
                                   (setf ,collector
                                         (setf ,tail (list thing))))))
        ,@body)
       (values-list ',collectors))))

(defun %with-collectors-check-collectors (collectors)
  "Check that all of the COLLECTORS are symbols. If not, raise an error."
  (let ((bad-collector (find-if-not #'symbolp collectors)))
    (when bad-collector
      (error 'type-error
             :datum bad-collector
             :expected-type 'symbol))))

(defun %with-collectors-gensyms-alist (collectors)
  "Return an alist mapping the symbols in COLLECTORS to gensyms"
  (mapcar #'cons collectors
          (mapcar (compose #'gensym
                           #'(lambda (x)
                               (format nil "~A-TAIL-" x)))
                  collectors)))

;; From ALEXANDRIA
(defun extract-function-name (spec)
  "Useful for macros that want to mimic the functional interface for functions
like #'eq and 'eq."
  (if (and (consp spec)
           (member (first spec) '(quote function)))
      (second spec)
      spec))

(defun generate-switch-body (whole object clauses test key &optional default)
  (with-gensyms (value)
    (setf test (extract-function-name test))
    (setf key (extract-function-name key))
    (when (and (consp default)
               (member (first default) '(error cerror)))
      (setf default `(,@default "No keys match in SWITCH. Testing against ~S with ~S."
                      ,value ',test)))
    `(let ((,value (,key ,object)))
      (cond ,@(mapcar (lambda (clause)
                        (if (member (first clause) '(t otherwise))
                            (progn
                              (when default
                                (error "Multiple default clauses or illegal use of a default clause in ~S."
                                       whole))
                              (setf default `(progn ,@(rest clause)))
                              '(()))
                            (destructuring-bind (key-form &body forms) clause
                              `((,test ,value ,key-form)
                                ,@forms))))
                      clauses)
            (t ,default)))))

(defmacro switch (&whole whole (object &key (test 'eql) (key 'identity))
                         &body clauses)
  "Evaluates first matching clause, returning its values, or evaluates and
returns the values of T or OTHERWISE if no keys match."
  (generate-switch-body whole object clauses test key))

(defmacro eswitch (&whole whole (object &key (test 'eql) (key 'identity))
                          &body clauses)
  "Like SWITCH, but signals an error if no key matches."
  (generate-switch-body whole object clauses test key '(error)))

(defmacro cswitch (&whole whole (object &key (test 'eql) (key 'identity))
                          &body clauses)
  "Like SWITCH, but signals a continuable error if no key matches."
  (generate-switch-body whole object clauses test key '(cerror "Return NIL from CSWITCH.")))

(defmacro xor (&rest datums)
  "Evaluates its arguments one at a time, from left to right. If more than one
argument evaluates to a true value no further DATUMS are evaluated, and NIL is
returned as both primary and secondary value. If exactly one argument
evaluates to true, its value is returned as the primary value after all the
arguments have been evaluated, and T is returned as the secondary value. If no
arguments evaluate to true NIL is returned as primary, and T as secondary
value."
  (with-gensyms (xor tmp true)
    `(let (,tmp ,true)
       (declare (ignorable ,tmp))
       (block ,xor
         ,@(mapcar (lambda (datum)
                     `(if (setf ,tmp ,datum)
                          (if ,true
                              (return-from ,xor (values nil nil))
                              (setf ,true ,tmp))))
                   datums)
         (return-from ,xor (values ,true t))))))

;; From ELEPHANT
(defmacro ifret (pred &body alt)
  "If pred is non-null, return the value, otherwise return the alternate value"
  (once-only (pred)
    `(if ,pred ,pred (progn ,@alt))))

;; ref: https://github.com/bendudson/array-operations
(defmacro nested-loop (syms dimensions &body body)
  "Iterates over a multidimensional range of indices.

   SYMS must be a list of symbols, with the first symbol
   corresponding to the outermost loop.

   DIMENSIONS will be evaluated, and must be a list of
   dimension sizes, of the same length as SYMS.

   Example:
    (nested-loop (i j) '(10 20) (format t \"~a ~a~%\" i j))"
  (unless syms (return-from nested-loop `(progn ,@body))) ; No symbols
  ;; Generate gensyms for dimension sizes
  (let* ((rank (length syms))
         ;; reverse our symbols list,
         ;; since we start from the innermost.
         (syms-rev (reverse syms))
         ;; innermost dimension first:
         (dims-rev (loop for i from 0 below rank
                         collecting (gensym)))
         ;; start with innermost expression
         (result `(progn ,@body)))
    ;; Wrap previous result inside a loop for each dimension
    (loop for sym in syms-rev for dim in dims-rev do
         (unless (symbolp sym)
           (error "~S is not a symbol. First argument to nested-loop must be a list of symbols" sym))
         (setf result
               `(loop for ,sym from 0 below ,dim do
                     ,result)))
    ;; Add checking of rank and dimension types,
    ;; and get dimensions into gensym list.
    (let ((dims (gensym)))
      `(let ((,dims ,dimensions))
         (unless (= (length ,dims) ,rank)
           (error "Incorrect number of dimensions: Expected ~a but got ~a" ,rank (length ,dims)))
         (dolist (dim ,dims)
           (unless (integerp dim)
             (error "Dimensions must be integers: ~S" dim)))
         ;; dimensions reversed so that innermost is last:
         (destructuring-bind ,(reverse dims-rev) ,dims
           ,result)))))

;;; DESTRUCTURING-*CASE
(defun expand-destructuring-case (key clauses case)
  (once-only (key)
    `(if (typep ,key 'cons)
         (,case (car ,key)
           . ,(mapcar (lambda (clause)
                        (destructuring-bind ((keys . lambda-list) &body body) clause
                          `(,keys
                            (destructuring-bind ,lambda-list (cdr ,key)
                              . ,body))))
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
  "Combination of destructuring-bind and ccase."
  (expand-destructuring-case keyform clauses 'ccase))

(defmacro destructuring-ecase (keyform &body clauses)
  "Combination of destructuring-bind and ecase."
  (expand-destructuring-case keyform clauses 'ecase))

(dolist (name '(destructuring-ccase destructuring-ecase))
  (setf (documentation name 'function) (documentation 'destructuring-case 'function)))

;; from iolib
(defmacro multiple-value-case ((values &key (test 'eql)) &body body)
  (setf values (std/list:ensure-list values))
  (assert values () "Must provide at least one value to test")
  (labels ((%do-var (var val)
               (cond
                 ((and (symbolp var) (member var '("_" "*") :test #'string=))
                  t)
                 ((consp var)
                  `(member ,val ',var :test ',test))
                 (t
                  `(,test ,val ',var))))
             (%do-clause (c gensyms)
               (destructuring-bind (vals &rest code) c
                 (let* ((tests (remove t (mapcar #'%do-var (std/list:ensure-list vals) gensyms)))
                        (clause-test (if (> 2 (length tests))
                                         (first tests)
                                         `(and ,@tests))))
                   `(,clause-test ,@code))))
             (%do-last-clause (c gensyms)
               (when c
                 (destructuring-bind (%test &rest code) c
                   (if (member %test '(otherwise t))
                       `((t ,@code))
                       `(,(%do-clause c gensyms)))))))
      (let ((gensyms (mapcar (lambda (v) (gensym (string v)))
                             values)))
        `(let ,(mapcar #'list gensyms values)
           (declare (ignorable ,@gensyms))
           (cond ,@(append (mapcar (lambda (c) (%do-clause c gensyms))
                                   (butlast body))
                           (%do-last-clause (std/list:lastcar body) gensyms)))))))
