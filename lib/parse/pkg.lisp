(require :sb-cltl2)

(defpackage :parse/proto
  (:use :cl :std)
  (:export :parse :parser-condition :parser-error :simple-parser-error))

(defpackage :parse/lex
  (:nicknames :lex)
  (:use :cl :cl-ppcre :std :parse/proto)
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
  (:use :cl :parse/proto)
  (:import-from :std :memq :required-argument)
  (:export :make-production :make-grammar :make-parser :parse-with-lexer
           :define-grammar :define-parser
           :yacc-compile-warning :conflict-warning :conflict-summary-warning
           :yacc-runtime-error :yacc-parse-error :yacc-parse-error-terminal
           :yacc-parse-error-value :yacc-parse-error-expected-terminals))

(defpackage parse/bytes
  (:use :cl :parse/proto)
  (:import-from :sb-cltl2
   :variable-information)
  (:import-from :std :with-gensyms :once-only
   :ensure-cons :ignore-some-conditions :octet-vector :octet)
  (:import-from :sb-ext :string-to-octets :octets-to-string)
  (:export 
   :with-vector-parsing
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
   :bind*
   :matching
   :match-i
   :match?
   :match-case
   :match-i-case
   :match-failed))

(defpackage :parse/pratt
  (:use :cl :parse/proto)
  (:export :pratt-parser :next-precedence :parse-prefix :parse-infix))

;; FIX 2024-11-09: name conflict ADVANCE bytes vs lex
(uiop:define-package :parse
  (:use :cl :std)
  (:use-reexport :parse/proto :parse/lex :parse/yacc :parse/pratt))
