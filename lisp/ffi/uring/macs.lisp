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
