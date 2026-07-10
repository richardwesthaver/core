#!/usr/bin/env -S core --script
#|Simple Calculator using MATH|#
(using :parse :math (:std :readtable))
(init :log :level :trace)
;; TODO 2026-05-22: align parse/lex with yacc lexer generator
(define-parser *calc*
  (:start-symbol e)
  (:terminals (int id + - := [ ] { } * @ /))
  (e
   (e e +)
   (e e -)
   int id))
(parse-with-lexer 
 (lambda () (read-lisp-string (linedit :prompt "> ")))
 *calc*)
