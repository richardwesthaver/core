;;; lib/parse/tests.lisp --- Parser Tests

;;

;;; Code:
(defpackage :parse/tests
  (:use :cl :rt :std :parse))

(in-package :parse/tests)

(defsuite :parse)
(in-suite :parse)

(deftest lex ()
  (is (string=
       (with-lexer-environment ("<foo>")
         (when (char= #\< (consume))
           (consume-until (make-matcher (is #\>)))))
       "foo"))
  (is (string=
       (let ((q "baz"))
         (with-lexer-environment ("foo bar baz")
           (consume-until (make-matcher (is q)))))
       "foo bar ")))

(defun digitp (c) (member c '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)))

(defun simple-lexer (stream)
  (let ((c (read-char stream nil nil)))
    (cond
      ((null c) (values nil nil))
      ((member c '(#\Space #\Tab #\Newline)) (simple-lexer stream))
      ((member c '(#\+ #\- #\* #\/ #\( #\)))
       (let ((v (intern (string c))))
         (values v v)))
      ((digitp c)
       (let ((buffer (make-array 10 :element-type 'character
                                    :fill-pointer 0)))
         (do ((c c (read-char stream nil nil)))
             ((or (null c) (not (digitp c)))
              (unless (null c) (unread-char c stream))
              (values 'int (read-from-string buffer)))
           (vector-push-extend c buffer))))
      ((alpha-char-p c)
       (let ((buffer (make-array 10 :element-type 'character
                                    :fill-pointer 0)))
         (do ((c c (read-char stream nil nil)))
             ((or (null c) (not (alphanumericp c)))
              (unless (null c) (unread-char c stream))
              (values 'id (copy-seq buffer)))
           (vector-push-extend c buffer))))
      (t (error "Lexing error")))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun k-2-3 (a b c) (declare (ignore a c)) b))

(define-parser *left-expression-parser*
  (:start-symbol expression)
  (:terminals (int id + - * / |(| |)|))

  (expression
   (expression + term)
   (expression - term)
   term)

  (term
   (term * factor)
   (term / factor)
   factor)

  (factor
   id
   int
   (|(| expression |)| #'k-2-3)))

(define-parser *ambiguous-expression-parser*
  (:start-symbol expression)
  (:terminals (int id + - * / |(| |)|))
  (:muffle-conflicts (16 0))

  (expression
   (expression + expression)
   (expression - expression)
   (expression * expression)
   (expression / expression)
   id
   int
   (|(| expression |)| #'k-2-3)))

(define-parser *precedence-left-expression-parser*
  (:start-symbol expression)
  (:terminals (int id + - * / |(| |)|))
  (:precedence ((:left * /) (:left + -)))

  (expression
   (expression + expression)
   (expression - expression)
   (expression * expression)
   (expression / expression)
   id
   int
   (|(| expression |)| #'k-2-3)))

(define-parser *precedence-right-expression-parser*
  (:start-symbol expression)
  (:terminals (int id + - * / |(| |)|))
  (:precedence ((:right * /) (:right + -)))

  (expression
   (expression + expression)
   (expression - expression)
   (expression * expression)
   (expression / expression)
   id
   int
   (|(| expression |)| #'k-2-3)))

(define-parser *precedence-nonassoc-expression-parser*
  (:start-symbol expression)
  (:terminals (int id + - * / |(| |)|))
  (:precedence ((:nonassoc * /) (:nonassoc + -)))
  (expression
   (expression + expression)
   (expression - expression)
   (expression * expression)
   (expression / expression)
   id
   int
   (|(| expression |)| #'k-2-3)))

(deftest yacc ()
  (flet ((parse (parser e) 
           (with-input-from-string (s e)
             (parse-with-lexer #'(lambda () (simple-lexer s)) parser))))
    (let ((*package* (find-package :parse/tests)))
      (let ((e "(x+3)+y*z") (v '(("x" + 3) + ("y" * "z"))))
        (is (equal (parse *left-expression-parser* e) v))
        (is (equal (parse *precedence-left-expression-parser* e) v))
        (is (equal (parse *precedence-right-expression-parser* e) v))
        (is (equal (parse *precedence-nonassoc-expression-parser* e) v)))
      (let ((e "x+5/3*(12+y)/3+z"))
        (let ((v '(("x" + (((5 / 3) * (12 + "y")) / 3)) + "z")))
          (is (equal (parse *left-expression-parser* e) v))
          (is (equal (parse *precedence-left-expression-parser* e) v)))
        (let ((v '("x" + ((5 / (3 * ((12 + "y") / 3))) + "z"))))
          (is (equal (parse *precedence-right-expression-parser* e) v)))
        (let ((v '("x" + (5 / (3 * ((12 + "y") / (3 + "z")))))))
          (is (equal (parse *ambiguous-expression-parser* e) v)))
        (signals yacc-parse-error
          (parse *precedence-nonassoc-expression-parser* e)))
      (dolist (e '("5/3*(" "5/3)"))
        (signals yacc-parse-error
          (parse *left-expression-parser* e))
        (signals yacc-parse-error
          (parse *ambiguous-expression-parser* e))
        (signals yacc-parse-error
          (parse *precedence-left-expression-parser* e))
        (signals yacc-parse-error
          (parse *precedence-right-expression-parser* e))))))
