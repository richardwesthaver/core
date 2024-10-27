;;; uring/macs.lisp --- Macros

;;

;;; Code:
(in-package :uring)

(defmacro defalien-int (name &body args)
  `(progn
     (define-alien-routine ,name int ,@args)
     (export '(,name) :uring)))

(defmacro def-with-ring (name &body args)
  `(defalien-int ,name (ring (* io-uring)) ,@args))

(defvar *io-opcodes* nil)

(defmacro with-io-sqe ((var val) &body body)
  `(with-alien ((,var io-uring-sqe ,val))
     ,@body))

(defmacro with-new-io-sqe (var &body body)
  `(with-alien ((,var io-uring-sqe))
     ,@body))

(defmacro with-io-sqe-op ((var op val) &body body)
  `(with-io-sqe (,var ,val)
     (setf (slot ,var 'opcode) ,op)
     ,@body
     ,var))

(defmacro with-new-io-sqe-op ((var op) &body body)
  `(with-new-io-sqe ,var
     (setf (slot ,var 'opcode) ,op)
     ,@body
     ,var))

(defmacro with-io-cqe (var &body body)
  `(with-alien ((,var io-uring-cqe))
     ,@body))

(defmacro with-io-uring ((var &optional val) &body body)
  `(let ((,var ,(or val (make-alien io-uring))))
     ,@body))

(defmacro with-new-io-uring (var &body body)
  `(with-alien ((,var io-uring))
     ,@body))

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
       (defmethod build-from ((self ,struct-name) (from system-area-pointer) &key &allow-other-keys)
         (with-io-sqe-op (sqe ,const-name (sap-alien from (struct io-uring-sqe)))
           ,@builder))
       (pushnew ',alien-name *io-opcodes*)
       (export '(,struct-name ,(symbolicate "MAKE-" struct-name) ,const-name ,alien-name)))))
