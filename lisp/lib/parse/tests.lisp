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

(deftest yacc ())
