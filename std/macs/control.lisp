;;; std/macs/control.lisp --- Control Flow Macros

;;

;;; Code:
(in-package :std/macs)

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

;; Inspired by TRIVIA
;; (defmacro match (x &body body))
;; (defmacro ematch (x &body body))

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
