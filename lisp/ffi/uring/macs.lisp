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

(defmacro with-io-sqe (var &body body)
  `(with-alien ((,var io-uring-sqe))
     ,@body))

(defmacro with-io-sqe-op ((var op) &body body)
  `(with-io-sqe ,var
     (setf (slot ,var 'opcode) ,op)
     ,@body
     ,var))

(defmacro with-io-cqe (var &body body)
  `(with-alien ((,var io-uring-cqe))
     ,@body))
