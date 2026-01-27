;;; api.lisp --- API Macros

;; DEFAPI

;;; Code:
(in-package :std/prim)

;;; Taken from SWANK (which is Public Domain.)
(defmacro destructure-case (value &body patterns)
  "Dispatch VALUE to one of PATTERNS.
A cross between `case' and `destructuring-bind'.
The pattern syntax is:
  ((HEAD . ARGS) . BODY)
The list of patterns is searched for a HEAD `eq' to the car of
VALUE. If one is found, the BODY is executed with ARGS bound to the
corresponding values in the CDR of VALUE."
  (let ((operator (gensym "op-"))
        (operands (gensym "rand-"))
        (tmp (gensym "tmp-")))
    `(let* ((,tmp ,value)
            (,operator (car ,tmp))
            (,operands (cdr ,tmp)))
       (case ,operator
         ,@(loop for (pattern . body) in patterns collect
                    (if (eq pattern t)
                        `(t ,@body)
                        (destructuring-bind (op &rest rands) pattern
                          `(,op (destructuring-bind ,rands ,operands
                                  ,@body)))))
         ,@(if (eq (caar (last patterns)) t)
               '()
               `((t (error "destructure-case failed: ~S" ,tmp))))))))

;;; Taken from Alexandria (which is Public Domain, or BSD.)

(define-condition simple-style-warning (simple-warning style-warning)
  ())

(defun simple-style-warn (format-control &rest format-args)
  (warn 'simple-style-warning
        :format-control format-control
        :format-arguments format-args))

(define-condition simple-program-error (simple-error program-error)
  ())

(defun simple-program-error (message &rest args)
  (error 'simple-program-error
         :format-control message
         :format-arguments args))

(defun required-argument (&optional name)
  "Signals an error for a missing argument of NAME. Intended for
use as an initialization form for structure and class-slots, and
a default value for required keyword arguments."
  (error "Required argument ~@[~S ~]missing." name))

(declaim (inline ensure-function))	; to propagate return type.
(declaim (ftype (function (t) (values function &optional))
                ensure-function))
(defun ensure-function (function-designator)
  "Returns the function designated by FUNCTION-DESIGNATOR:
if FUNCTION-DESIGNATOR is a function, it is returned, otherwise
it must be a function name and its FDEFINITION is returned."
  (if (functionp function-designator)
      function-designator
      (fdefinition function-designator)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-body (body &key documentation whole)
    "Parses BODY into (values remaining-forms declarations doc-string).
Documentation strings are recognized only if DOCUMENTATION is true.
Syntax errors in body are signalled and WHOLE is used in the signal
arguments when given."
    (let ((doc nil)
          (decls nil)
          (current nil))
      (tagbody
       :declarations
         (setf current (car body))
         (when (and documentation (stringp current) (cdr body))
           (if doc
               (error "Too many documentation strings in ~S." (or whole body))
               (setf doc (pop body)))
           (go :declarations))
         (when (and (listp current) (eql (first current) 'declare))
           (push (pop body) decls)
           (go :declarations)))
      (values body (nreverse decls) doc))))

(defmacro define-api (name lambda-list type-list &body body)
  (flet ((parse-type-list (type-list)
           (let ((ret (lastcar type-list)))
             (assert ret () "You forgot to specify return type.")
             (values (nbutlast type-list)
                     `(values ,@(when ret `(,ret)) &optional)))))
    (multiple-value-bind (body decls docstring)
        (parse-body body :documentation t :whole `(define-api ,name))
      (multiple-value-bind (arg-typespec value-typespec)
          (parse-type-list type-list)
        (multiple-value-bind (bits reqs opts rest keys) (parse-lambda-list lambda-list)
          (declare (ignore bits) (ignorable reqs opts rest keys))
          `(progn
             (declaim (ftype (function ,arg-typespec ,value-typespec) ,name))
             (locally
                 ;; Muffle the annoying "&OPTIONAL and &KEY found in
                 ;; the same lambda list" style-warning
                 #+sbcl (declare (sb-ext:muffle-conditions style-warning))
                 (defun ,name ,lambda-list
                   ,docstring
                   ,@decls
                   (locally
                       #+sbcl (declare (sb-ext:unmuffle-conditions style-warning))
                       ;; SBCL will interpret the ftype declaration as
                       ;; assertion and will insert type checks for us.
                       ,@body)))))))))

;; (defmacro defapi (name))
