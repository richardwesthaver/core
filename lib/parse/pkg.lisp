;;; parse/pkg.lisp --- Parser packages

;;; Code:
(defpkg :parse/proto
  (:use :cl :std)
  (:export :parse :parser-condition :parser-error :simple-parser-error))

(defpkg :parse/bytes
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

(defpkg :parse/lex
  (:nicknames :lex)
  (:use :std-lisp :parse/proto)
  (:shadowing-import-from :parse/bytes :advance :peek)
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

(defpkg :parse/yacc
  (:use :cl :parse/proto)
  (:import-from :std :memq :required-argument)
  (:export :make-production :make-grammar :make-parser :parse-with-lexer
           :define-grammar :define-parser
           :yacc-compile-warning :conflict-warning :conflict-summary-warning
           :yacc-runtime-error :yacc-parse-error :yacc-parse-error-terminal
           :yacc-parse-error-value :yacc-parse-error-expected-terminals))

(defpkg :parse/pratt
  (:use :cl :parse/proto)
  (:export :pratt-parser :next-precedence :parse-prefix :parse-infix))
