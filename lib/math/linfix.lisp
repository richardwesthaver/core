;;; linparse.lisp --- Lisp Infix Parser

;; 

;;; Commentary:

;; This is an implementation of an infix reader macro. It should run in any
;; valid Common Lisp and has been tested in Allegro CL 4.1, Lucid CL 4.0.1,
;; MCL 2.0 and CMU CL. It allows the user to type arithmetic expressions in
;; the traditional way (e.g., 1+2) when writing Lisp programs instead of
;; using the normal Lisp syntax (e.g., (+ 1 2)).  It is not intended to be a
;; full replacement for the normal Lisp syntax. If you want a more complete
;; alternate syntax for Lisp, get a copy Apple's MLisp or Pratt's CGOL.
;;
;; Although similar in concept to the Symbolics infix reader (#<DIAMOND>), 
;; no real effort has been made to ensure compatibility beyond coverage 
;; of at least the same set of basic arithmetic operators. There are several 
;; differences in the syntax beyond just the choice of #I as the macro 
;; character. (Our syntax is a little bit more C-like than the Symbolics 
;; macro in addition to some more subtle differences.) 
;;
;; We initially chose $ as a macro character because of its association
;; with mathematics in LaTeX, but unfortunately that character is already
;; used in MCL. We switched to #I() because it was one of the few options
;; remaining.
;;
;; Written by Mark Kantrowitz, School of Computer Science,
;; Carnegie Mellon University, March 1993.
;;
;; Copyright (c) 1993 by Mark Kantrowitz. All rights reserved.

;;;; Syntax:
;;   Begin the reader macro with #I( and end it with ). For example,
;;      #I( x^^2 + y^^2 )
;;   is equivalent to the Lisp form
;;      (+ (expt x 2) (expt y 2))
;;   but much easier to read according to some folks.
;;
;;   If you want to see the expansion, type a quote before the #I form
;;   at the Lisp prompt:
;;     > '#I(if x<y<=z then f(x)=x^^2+y^^2 else f(x)=x^^2-y^^2)
;;     (IF (AND (< X Y) (<= Y Z))
;;         (SETF (F X) (+ (EXPT X 2) (EXPT Y 2)))
;;         (SETF (F X) (- (EXPT X 2) (EXPT Y 2))))

;;;; Operators:
;;    NOTE: == is equality, = is assignment (C-style).
;;
;;     \                   quoting character:  x\-y  -->  x-y
;;     !                   lisp escape    !(foo bar) -->  (foo bar)
;;     ;                   comment
;;     x = y               assignment                     (setf x y)
;;     x += y              increment                      (incf x y)
;;     x -= y              decrement                      (decf x y)
;;     x *= y              multiply and store             (setf x (* x y))
;;     x /= y              divide and store               (setf x (/ x y))
;;     x|y                 bitwise logical inclusive or   (logior x y)
;;     x^y                 bitwise logical exclusive or   (logxor x y)
;;     x&y                 bitwise logical and            (logand x y)
;;     x<<y                left shift                     (ash x y)
;;     x>>y                right shift                    (ash x (- y))
;;     ~x                  ones complement (unary)        (lognot x)
;;     x and y             conjunction                    (and x y)
;;     x && y              conjunction                    (and x y)
;;     x or y              disjunction                    (or x y)
;;     x || y              disjunction                    (or x y)
;;     not x               negation                       (not x)
;;     x^^y                exponentiation                 (expt x y)
;;     x,y                 sequence                       (progn x y)
;;     (x,y)               sequence                       (progn x y)
;;                         also parenthesis (x+y)/z -->   (/ (+ x y) z)
;;     f(x,y)              functions                      (f x y)
;;     a[i,j]              array reference                (aref a i j)
;;     x+y x*y             arithmetic                     (+ x y) (* x y) 
;;     x-y x/y             arithmetic                     (- x y) (/ x y) 
;;     -y                  value negation                 (- y)
;;     x % y               remainder                      (mod x y)
;;     x<y x>y             inequalities                   (< x y) (> x y)
;;     x <= y  x >= y      inequalities                   (<= x y) (>= x y)
;;     x == y              equality                       (= x y) 
;;     x != y              equality                       (not (= x y))
;;     if p then q         conditional                    (when p q)
;;     if p then q else r  conditional                    (if p q r) 

;;;; Precedence:
;;    The following precedence conventions are obeyed by the infix operators:
;;      [ ( !
;;      ^^
;;      ~
;;      * / %
;;      + -
;;      << >>
;;      < == > <= != >=
;;      &
;;      ^
;;      |
;;      not
;;      and
;;      or
;;      = += -= *= /=
;;      , 
;;      if
;;      then else
;;      ] )
;;
;;    Note that logical negation has lower precedence than numeric comparison
;;    so that "not a<b" becomes (not (< a b)), which is different from the
;;    C precedence conventions. You can change the precedence conventions by
;;    modifying the value of the variable *operator-ordering*.

;;; Code:
(in-package :math)

(defparameter *operator-tokens*
  `(("^" ^) ("**" **)
    ("./" ./) ("/" /)
    ("*" *) (".*" .*) ("@" @)
    (".+" +) ("+" +)
    (".-" -) ("-" -)
    ("(" \() (")" \))
    ("[" \[) ("]" \])
    (":" |:|)
    ("=" =) ("==" ==)
    ("," \,)
    ("'" ctranspose) (".'" transpose)))

(defun find-token (str stream)
  (let ((stack nil))
    (loop for r.i across str
          for m.i = (read-char stream t nil t)
          do (push m.i stack)
          when (char/= r.i m.i)
             do (progn
                  (map nil #'(lambda (x) (unread-char x stream)) stack)
                  (return nil))
          finally (return t))))

(defun token-reader (stream &optional (enclosing-chars '(#\( . #\))))
  (let* ((stack nil)
         (expr nil)
         (lspe nil))
    (labels ((read-stack (&optional (empty? t))
               (let* ((fstack (reverse (remove-if #'(lambda (x) (member x *whitespaces*)) stack)))
                      (tok (and fstack (read-from-string (coerce fstack 'string)))))
                 (prog1 tok
                   (when empty?
                     (when fstack (push tok expr))
                     (setf stack nil))))))
      (loop for c = (peek-char nil stream t nil t)
            while c
            summing (cond ((char= c (cdr enclosing-chars)) -1) ((char= c (car enclosing-chars)) +1) (t 0)) into count
            do (cond
                 ((and (char= c (cdr enclosing-chars)) (= count -1))
                  (read-char stream t nil t)
                  (read-stack)
                  (return (values (reverse expr) lspe)))
                 ((member c '(#\# #\\))
                  (when (char= c #\\) (read-char stream t nil t))
                  (let ((word (read stream))
                        (sym (gensym)))
                    (push sym expr)
                    (push (list sym word) lspe)))
                 ((when-let ((tok (find-if #'(lambda (x) (find-token (first x) stream)) (sort (remove-if-not #'(lambda (x) (char= c (aref (first x) 0))) *operator-tokens*) #'> :key #'(lambda (x) (length (first x)))))))
                    (read-stack)
                    (push (second tok) expr)))
                 ((and (char= c #\i) (numberp (read-stack nil)))
                  (read-char stream t nil t)
                  (push (complex 0 (read-stack nil)) expr)
                  (setf stack nil))
                 ((member c *whitespaces*)
                  (read-char stream t nil t)
                  (read-stack))
                 ((char= c #\\)
                  (read-char stream t nil t)
                  (read-stack)
                  (push (read stream t nil nil) expr))
                 (t
                  (push (read-char stream t nil t) stack)))))))

(defun list-lexer (list)
  #'(lambda () (if (null list) (values nil nil)
                   (let* ((value (pop list)))
                     (values (cond ((member value *operator-tokens* :key #'second) value)
                                   ((numberp value) 'number)
                                   ((symbolp value) 'id)
                                   (t (error "Unexpected value ~S" value)))
                             value)))))

(define-parser *linfix-parser*
  (:start-symbol expr)
  (:terminals (** ./ / * .* @ ^ + - = == |(| |)| [ ] |:| |,| ctranspose transpose id number))
  (:precedence ((:left ctranspose transpose)
                (:right **)
                (:left ./ / * .* @ ^)
                (:left + -)
                (:left = ==)))
  (expr
   (expr ctranspose #'(lambda (a b) (list b a)))
   (expr transpose #'(lambda (a b) (list b a)))
   (expr + expr #'(lambda (a b c) (list b a c)))
   (expr - expr #'(lambda (a b c) (list b a c)))
   (expr / expr #'(lambda (a b c) (list b a c)))
   (expr ./ expr #'(lambda (a b c) (list b a c)))
   (expr * expr #'(lambda (a b c) (list b a c)))
   (expr .* expr #'(lambda (a b c) (list b a c)))
   (expr @ expr #'(lambda (a b c) (list b a c)))
   (expr ^ expr #'(lambda (a b c) (list b a c)))
   (expr ** expr #'(lambda (a b c) (list b a c)))
   (expr = expr #'(lambda (a b c) (declare (ignore b)) (list 'setf a c)))
   (expr == expr #'(lambda (a b c) (list b a c)))
   callable slice
   term)
  ;;
  (args
   (expr #'list)
   (expr |,| args #'(lambda (a b c) (declare (ignore b)) (if (consp c) (list* a c) (list a c)))))
  (callable
   (term |(| |)| #'(lambda (a b c) (declare (ignore b c)) (list a)))
   (term |(| args |)| #'(lambda (a b c d) (declare (ignore b d)) (list* a c))))
  ;;
  (idxs
   expr
   (expr |:| expr #'(lambda (a b c) (declare (ignore b)) (list :slice a c)))
   (expr |:| expr |:| expr #'(lambda (a b c d e) (declare (ignore b d)) (list :slice a c e))))
  (sargs
   (idxs #'list)
   (idxs |,| sargs #'(lambda (a b c) (declare (ignore b)) (if (consp c) (list* a c) (list a c)))))
  (slice
   (term [ ] #'(lambda (a b c) (declare (ignore b c)) (list 'generic-ref a)))
   (term [ sargs ] #'(lambda (a b c d) (declare (ignore b d)) (list* 'generic-ref a c))))
  ;;
  (term
   number
   id
   (- term)
   (/ term)
   (./ term)
   (|(| expr |)| #'(lambda (a b c) (declare (ignore a c)) b))))

(defparameter *ref-list* '((cons elt) (array aref) (obj/tensor::base-tensor obj/tensor::ref)))

(defun process-slice (args)
  (mapcar #'(lambda (x) (if (and (consp x) (eql (car x) :slice)) `(list* ,@(cdr x)) x)) args))

(defmacro generic-ref (x &rest args)
  (cond
    ((null args) x)
    ((find-if #'(lambda (sarg) (and (consp sarg) (eql (car sarg) ':slice))) args)
     `(obj/tensor::subtensor~ ,x (list ,@(process-slice args))))
    (t
     `(etypecase ,x
        ,@(mapcar #'(lambda (l) `(,(car l) (,(cadr l) ,x ,@args))) (if (> (length args) 1) (cdr *ref-list*) *ref-list*))))))

(define-setf-expander generic-ref (x &rest args &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion x env)
    (with-gensyms (store)
      (values (append dummies newval)
              (append vals (list getter))
              `(,store)
              (let ((arr (car newval)))
                `(prog1 ,(cond
                          ((null args)
                           `(obj/tensor::copy! ,store ,arr))
                          ((find-if #'(lambda (sarg) (and (consp sarg) (eql (car sarg) ':slice))) args)
                           `(setf (obj/tensor::subtensor~ ,arr (list ,@(process-slice args))) ,store))
                          (t`(etypecase ,arr
                               ,@(mapcar #'(lambda (l) `(,(car l) (setf (,(cadr l) ,arr ,@args) ,store))) (if (> (length args) 1) (cdr *ref-list*) *ref-list*)))))
                   ,setter))
              `(generic-ref ,getter ,@args)))))
