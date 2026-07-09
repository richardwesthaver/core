;;; errors.lisp --- PPCRE Errors

;;; Code:
(in-package :ppcre)

(defvar *syntax-error-string* nil
  "The string which caused the syntax error.")

(define-condition ppcre-error (simple-error)
  ()
  (:documentation "All errors signaled by PPCRE are of
this type."))

(define-condition ppcre-syntax-error (ppcre-error)
  ((string :initarg :string
           :reader ppcre-syntax-error-string)
   (pos :initarg :pos
        :reader ppcre-syntax-error-pos))
  (:default-initargs
      :pos nil
      :string *syntax-error-string*)
  (:report (lambda (condition stream)
             (format stream "~?~@[ at position ~A~]~@[ in string ~S~]"
                     (simple-condition-format-control condition)
                     (simple-condition-format-arguments condition)
                     (ppcre-syntax-error-pos condition)
                     (ppcre-syntax-error-string condition))))
  (:documentation "Signaled if PPCRE's parser encounters an error
when trying to parse a regex string or to convert a parse tree into
its internal representation."))

(setf (documentation 'ppcre-syntax-error-string 'function)
      "Returns the string the parser was parsing when the error was
encountered \(or NIL if the error happened while trying to convert a
parse tree).")

(setf (documentation 'ppcre-syntax-error-pos 'function)
      "Returns the position within the string where the error occurred
\(or NIL if the error happened while trying to convert a parse tree")

(define-condition ppcre-invocation-error (ppcre-error)
  ()
  (:documentation "Signaled when PPCRE functions are
invoked with wrong arguments."))

(defmacro signal-syntax-error* (pos format-control &rest format-arguments)
  `(error 'ppcre-syntax-error
          :pos ,pos
          :format-control ,format-control
          :format-arguments (list ,@format-arguments)))

(defmacro signal-syntax-error (format-control &rest format-arguments)
  `(signal-syntax-error* nil ,format-control ,@format-arguments))

(defmacro signal-invocation-error (format-control &rest format-arguments)
  `(error 'ppcre-invocation-error
          :format-control ,format-control
          :format-arguments (list ,@format-arguments)))
