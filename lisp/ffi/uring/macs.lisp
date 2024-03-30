;;; uring/macs.lisp --- Macros

;;

;;; Code:
(in-package :uring)

(defmacro defalien-int (name &body args)
  `(progn
     (define-alien-routine ,name int ,@args)
     (export '(,name) :uring)))

(defmacro def-with-ring (name &body args)
  `(defalien-int ,name (ring (* (struct io-uring))) ,@args))

(defvar *io-opcodes* nil)

;; io_uring_prep_*
(defmacro def-io-op (val name slots &body builder)
  "Define a wrapper for an io-uring opcode. This macro will create a
structure class with NAME and SLOTS. BUILDER is the body of the BUILD
method for this struct, with CONST bound to VAR."
  (let ((struct-name (symbolicate "IO-OP-" name))
        (const-name (symbolicate "+IO-" name "+"))
        (alien-name (symbolicate "IORING-OP-" name)))
    `(progn
       (defconstant ,const-name ,val)
       (defstruct ,struct-name ,@slots)
       (defmethod build ((self ,struct-name) &key &allow-other-keys)
         ,@builder)
       (pushnew ',alien-name *io-opcodes*))))

(defmacro def-alien-enum-with (name list)
  `(define-alien-type ,name
       (enum ,name
             ,@(symbol-value list))))
