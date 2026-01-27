;;; api.lisp --- API Macros

;; DEFAPI

;;; Code:
(in-package :std/prim)

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
