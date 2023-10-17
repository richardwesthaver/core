;;; sym.lisp --- Symbol utils

;; inspired by alexandria/symbols.lisp

;;; Code:
(uiop:define-package :std/sym
  (:use :cl :std/str :sb-int)
  (:export
   #:ensure-symbol
   #:format-symbol
   #:make-keyword
   #:make-slot-name
   #:make-gensym
   #:make-gensym-list
   #:with-gensyms
   #:with-unique-names
   #:symbolicate))

(in-package :std/sym)

;; (reexport-from :sb-int
;; 	       :include '(:with-unique-names :symbolicate :package-symbolicate :keywordicate :gensymify*))

;; On SBCL, `with-unique-names' is defined under
;; src/code/primordial-extensions.lisp. We use that instead of
;; defining our own.
(setf (macro-function 'with-gensyms) (macro-function 'with-unique-names))

(declaim (inline ensure-symbol))
(defun ensure-symbol (name &optional (package *package*))
  "Returns a symbol with name designated by NAME, accessible in package
designated by PACKAGE. If symbol is not already accessible in PACKAGE, it is
interned there. Returns a secondary value reflecting the status of the symbol
in the package, which matches the secondary return value of INTERN.

Example:

  (ensure-symbol :cons :cl) => cl:cons, :external
"
  (intern (string name) package))

(defun maybe-intern (name package)
  (values
   (if package
       (intern name (if (eq t package) *package* package))
       (make-symbol name))))

(declaim (inline format-symbol))
(defun format-symbol (package control &rest arguments)
  "Constructs a string by applying ARGUMENTS to string designator CONTROL as
if by FORMAT within WITH-STANDARD-IO-SYNTAX, and then creates a symbol named
by that string.

If PACKAGE is NIL, returns an uninterned symbol, if package is T, returns a
symbol interned in the current package, and otherwise returns a symbol
interned in the package designated by PACKAGE."
  (maybe-intern (with-standard-io-syntax
                  (apply #'format nil (string control) arguments))
                package))

(defun make-keyword (name)
  "Interns the string designated by NAME in the KEYWORD package."
  (intern (string name) :keyword))

(defmacro make-slot-name (name)
  "make slot-name"
  `(intern ,(string-upcase name) :keyword))

(defun make-gensym (name)
  "If NAME is a non-negative integer, calls GENSYM using it. Otherwise NAME
must be a string designator, in which case calls GENSYM using the designated
string as the argument."
  (gensym (if (typep name '(integer 0))
              name
              (string name))))

(sb-ext:with-unlocked-packages (:sb-int)
  (handler-bind
      ((sb-kernel:redefinition-warning #'muffle-warning))
    (defun make-gensym-list (length &optional (x "G"))
      "Returns a list of LENGTH gensyms, each generated as if with a call to
MAKE-GENSYM, using the second (optional, defaulting to \"G\")
argument. This function is implemented in SBCL
src/code/primordial-extensions.lisp but re-implemented here. The only
difference is that we also handle non-zero integers, which can be
passed as the first argument to `gensym'."
      (let ((g (if (typep x '(integer 0)) x (string x))))
	(loop repeat length
              collect (gensym g))))))
