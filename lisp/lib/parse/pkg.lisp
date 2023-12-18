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

(defpackage :parse/lalr
  (:nicknames :lalr)
  (:use :cl :std)
  (:export 
   :make-parser :define-grammar
   :*lalr-debug* :lalr-parse))

(uiop:define-package :parse
    (:use :cl :std)
  (:use-reexport :parse/lex :parse/lalr))
