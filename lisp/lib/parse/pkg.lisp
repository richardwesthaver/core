(require :sb-cltl2)

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
  (:use :cl :std)
  (:export :make-production :make-grammar :make-parser :parse-with-lexer
           :define-grammar :define-parser
           :yacc-compile-warning :conflict-warning :conflict-summary-warning
           :yacc-runtime-error :yacc-parse-error :yacc-parse-error-terminal
           :yacc-parse-error-value :yacc-parse-error-expected-terminals))

(defpackage parse/bytes
  (:use :cl :babel)
  (:import-from :sb-cltl2
   :variable-information)
  (:import-from :std :with-gensyms :once-only
   :ensure-cons :ignore-some-conditions :octet-vector :octet)
  (:export :with-vector-parsing
           :with-string-parsing
           :with-octets-parsing
           :eofp
           :current
           :peek
           :eof-value
           :pos
           :advance
           :advance*
           :advance-to
           :advance-to*
           :skip
           :skip*
           :skip+
           :skip?
           :skip-until
           :skip-while
           :bind
           :match
           :match-i
           :match?
           :match-case
           :match-i-case
           :match-failed))

(uiop:define-package :parse
    (:use :cl :std)
  (:use-reexport :parse/lex :parse/yacc))
