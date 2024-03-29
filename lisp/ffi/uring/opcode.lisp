;;; uring/opcode.lisp --- Opcodes

;; Wrapper for opcodes defined in liburing/io_uring.h. 

;;; Commentary:

;; 

;;; Code:
(in-package :uring)

(defmacro def-io-op ((name (var const)) slots &body builder)
  "Define a wrapper for an io-uring opcode. This macro will create a
structure class with NAME and SLOTS. BUILDER is the body of the BUILD
method for this struct, with CONST bound to VAR."
  `(progn
     (defstruct ,name ,@slots)
     (defmethod build ((self ,name) &key &allow-other-keys)
       (let ((,var ,const))
         ,@builder))))
