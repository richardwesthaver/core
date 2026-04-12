;;; uring/macs.lisp --- Macros

;;

;;; Code:
(in-package :uring)

(defmacro defalien-int (name &body args)
  `(progn
     (defar ,name int ,@args)))

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
(defmacro define-io-op (val name slots &body builder)
  "Define a wrapper for an io-uring opcode. This macro will create a
structure class with NAME and SLOTS. BUILDER is the body of the BUILD
method for this struct, with CONST bound to VAR.

SELF, FROM and SQE are all exposed for use in BUILDER."
  (let ((struct-name (symbolicate "IO-OP-" name))
        (const-name (symbolicate "+IO-" name "+"))
        (alien-name (symbolicate "IORING-OP-" name)))
    `(progn
       (defconstant ,const-name ,val)
       (defstruct ,struct-name ,@slots)
       (defmethod build ((self ,struct-name) &key)
         (build-from self (make-alien io-uring-sqe)))
       (defmethod build-from ((self ,struct-name) (from alien-value) &key &allow-other-keys)
         (with-io-sqe-op (sqe ,const-name from)
           ,@builder))
       (defmethod build-from ((self ,struct-name) (from system-area-pointer) &key &allow-other-keys)
         (build-from self (io-uring-get-sqe from)))
       (pushnew ',alien-name *io-opcodes*))))

(defmacro def-io-op (val name slots &body builder)
  `(define-io-op ,val ,name ,slots
     (with-slots ,(mapcar 'ensure-car slots) self
       ,@builder)))

