;;; js/pkg.lisp --- Javascript Code Generator

;; Lisp -> Javascript

;;; Commentary:

;; The current state-of-the-art CL->JS transpiler is Parenscript which hasn't
;; been officially patched in many years. This module is a port of Parenscript
;; using our SYN/GEN machinery which more closely resembles C-MERA.

;; In addition to JS.LISP file support, our other main goal is to support a
;; WITH-JS macro which is designed to work well with the WITH-HTML and
;; WITH-CSS macros. For embedding JS documents and snippets into a stream.

;; ref: https://parenscript.common-lisp.dev

;;; Code:
(defpackage :syn/gen/js
  (:nicknames :gen/js)
  (:use :cl :syn/gen :dat/html :dat/css)
  (:export
   #:*minify-js*
   #:*js-output*
   #:js-output-stream))

(pkg:defpackage* :syn/gen/js/sym
  ()
  (:nicknames :js)
  (:use :cl)
  (:import-from :syn/gen :quoty :print-code :write-code :gen-package :code-print :cl-reader))

(in-package :syn/gen/js)

(defvar *js-symbols*
  '( ; operators
    ;; arithmetic
    +
    unary-plus
    -
    negate
    *
    /
    %
    ;; bitwise
    &
    \|
    ^
    ~
    >>
    <<
    >>>
    ;; assignment
    =
    +=
    -=
    *=
    /=
    %=
    &=
    \|=
    ^=
    ~=
    >>=
    <<=
    >>>=
    ;; increment/decrement
    ++
    --
    post++
    post--
    ;; comparison
    ==
    ===
    !=
    !==
    >
    >=
    <
    <=
    ;; logical
    &&
    \|\|
    !
    ;; misc
    ? ;; ternary
    |,|
    delete
    function
    get
    set
    in
    instanceof
    new
    typeof
    void
    ;; literals
    nil
    t
    false
    undefined
    this
    ;; statements
    block
    break
    continue
    do-while  ; currently unused
    for
    for-in
    if
    label
    return
    switch
    default
    throw
    try
    var
    while
    with
    array
    aref
    cond
    lambda
    defun
    object
    getprop
    funcall
    escape
    regex))

(defvar *js-syntax*
  '(t ;; literals
    nil
    ;; array literals
    array
    list
    aref
    elt
    make-array
    []
    ;; operators
    ;; logical boolean
    not
    and
    or
    ;; bitwise boolean
    logand
    logior
    logxor
    lognot
    ash
    *
    /
    rem
    mod
    +
    -
    <
    >
    <=
    >=
    incf
    decf
    equal
    eql
    eq
    =
    ;; compile-time stuff
    eval-when
    ;; body forms
    progn
    ;; if
    if
    when
    unless
    ;; control flow
    return
    return-from
    throw
    ;; assignment and binding
    setf
    defsetf
    psetf
    setq
    psetq
    let*
    let
    ;; variables
    defvar
    ;; iteration
    do
    do*
    dotimes
    dolist
    loop
    ;; case
    switch
    case
    default
    ;; function definition
    defun
    lambda
    flet
    labels
    ;; lambda lists
    &key
    &rest
    &body
    &optional
    &aux
    &environment
    &key-object
    ;; macros
    macrolet
    symbol-macrolet
    define-symbol-macro
    define-ps-symbol-macro
    defmacro
    ;; utils
    max
    min
    floor
    ceiling
    round
    sin
    cos
    tan
    asin
    acos
    atan
    pi
    sinh
    cosh
    tanh
    asinh
    acosh
    atanh
    1+
    1-
    abs
    evenp
    oddp
    exp
    expt
    log
    sqrt
    random
    ignore-errors
    concatenate
    length
    stringp
    numberp
    functionp
    append
    apply
    destructuring-bind))

(define-gen-backend :js :syn/gen/js :sym :syn/gen/js/sym)

(defclass js-output-stream (std:wrapped-stream) ()
  (:default-initargs :stream (make-synonym-stream '*standard-output*)))

(defvar *js-output* (make-instance 'js-output-stream))

(defvar *minify-js* nil
  "When Non-nil, minify JS output.")
