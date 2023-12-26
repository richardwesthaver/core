(defpackage :parse/lex
  (:nicknames :lex)
  (:use :cl :cl-ppcre :std)
  (:export
   #:*string*
   #:*length*
   #:*index*
   #:with-lexer-environment
   #:consume
   #:advance
   #:unread
   #:peek
   #:advance-n
   #:unread-n
   #:consume-until
   #:matcher-character
   #:matcher-string
   #:matcher-range
   #:matcher-find
   #:matcher-or
   #:matcher-and
   #:matcher-not
   #:matcher-next
   #:matcher-prev
   #:matcher-any
   #:make-matcher
   #:define-matcher))

(defpackage :parse/yacc
  (:use :common-lisp)
  (:export :make-production :make-grammar :make-parser :parse-with-lexer
           :define-grammar :define-parser
           :yacc-compile-warning :conflict-warning :conflict-summary-warning
           :yacc-runtime-error :yacc-parse-error :yacc-parse-error-terminal
           :yacc-parse-error-value :yacc-parse-error-expected-terminals))

(uiop:define-package :parse
    (:use :cl :std)
  (:use-reexport :parse/lex :parse/lalr))
