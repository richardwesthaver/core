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
(in-package :syn/linparse)
