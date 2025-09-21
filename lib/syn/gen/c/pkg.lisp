;;; c/pkg.lisp --- C Code Generator

;; Lisp -> C

;; Commentary:

;; There are quite a few C Code Generators in the Common Lisp ecosystem, and
;; of course ECL which is itself a source-to-source Lisp implementation which
;; targets C. This one is closer to c-mera.

;; ref: https://github.com/kiselgra/c-mera

;; ref: https://selgrad.org/publications/2014_els_SLWLS.pdf

;; ref: https://selgrad.org/publications/2017_els_LSS.pdf

;; ref: https://github.com/gcc-mirror/gcc/tree/master/gcc/c

;;; Code:
(defpackage :syn/gen/c
  (:nicknames :gen/c)
  (:use :cl :syn/gen :std/pipe :std/seq :std/meta :cli/tools/cc :cli/env :id :ast)
  ;; (:shadowing-import-from :cl :type :float)
  (:export
   #:*c-backend*
   #:split-aref
   #:split-pref
   #:split-deref
   #:split-oref
   #:split-addr
   #:split-unary
   #:read-float
   #:fix-case
   #:dissect
   #:pre-process
   #:pre-process-heads
   #:*c-swap*
   #:*c-exports*
   #:*c-syntax*
   #:*c-symbols*
   #:c-reader
   #:c-processor
   #:read-gen-c-file
   #:read-gen-c-string
   #:assignment-expression
   #:infix-expression
   #:prefix-expression
   #:postfix-expression
   #:not-expression
   #:conditional-expression
   #:cast-expression
   #:jump-statement
   #:label-statement
   #:expression-statement
   #:compound-statement
   #:if-statement
   #:for-statement
   #:while-statement
   #:do-statement
   #:comment
   #:switch-case-statement
   #:switch-case-item
   #:attribute-expression
   #:typedef
   #:include
   #:preprocessor-macro
   #:c-syntax
   #:make-exprs
   #:make-block
   #:make-simple-block
   #:switch-reader
   #:decompose-declaration
   #:else-if-traverser
   #:nested-ast-remover
   #:renamer
   #:decl-blocker
   #:if-blocker
   #:simple-print
   #:gen-reader
   #:gen-c
   #:c-reader-switch))

(in-package :syn/gen/c)

(defvar *c-backend*
  (append *cl-symbols*
          '(name preprocessor-macro include typedef
            chars comment do-statement while-statement init
            for-statement else-body if-body if-statement
            statements compound-statement expression semicolon
            expression-statement kind jump-statement cast-expression
            else then test conditional-expression not-expression
            postfix-expression prefix-expression infix-expression
            operator assignment-expression function-pointer float
            pointer pointer-reference component object
            object-reference indizes array array-reference items
            clist value type specifier declaration-item
            bindings braces declaration-list union-definition
            members struct-definition parameters parameter-list
            body parameter function-definition enum-definition
            declaration-value float-type linebreak
            c-type
            constant attribute-expression switch cases
            switch-case-statement switch-case-item switch-case-item)))

(export *c-backend*)

(defparameter *c-symbols*
  '(and or not > <
    = /= <= >=
    + - * /
    do-while
    return break continue
    if cond when
    fn
    array aref
    deref addr
    union
    function
    progn block
    null length
    min max abs
    sin cos tan
    1- 1+
    type float-type
    funcall
    attribute))

(defparameter *c-syntax*
  '(set *= %= += -= >>= <<= &= ^= \|=
    == != \| \|\| % << >> ^ & && ~ ! ?
    switch
    prefix++ prefix--
    postfix-- postfix++ postfix*
    deref addr
    struct enum oref pref specifier
    include comment decl
    fpointer for while
    typedef cast sizeof
    goto label clist
    cpp pragma))

(defparameter *c-exports*
  (append *c-symbols*
          *c-syntax*
          *cl-symbols*))

(defparameter *c-swap*
  (append *c-symbols* *c-syntax*))

(pkg:defpackage* :syn/gen/c/swap
    (:shadow-symbols *c-swap*))

(pkg:defpackage* :syn/gen/c/sym
    (:shadow-symbols *c-swap* :export-symbols *c-exports*)
  (:nicknames :c)
  (:use :cl)
  (:import-from :syn/gen :quoty :print-code :write-code :cintern :gen-package :code-print :cl-reader)
  (:import-from :syn/gen/c :c-reader :read-gen-c-string :read-gen-c-file
   :switch-reader :decompose-declaration))

(define-gen-backend :c :syn/gen/c :sym :syn/gen/c/sym :swap :syn/gen/c/swap)
