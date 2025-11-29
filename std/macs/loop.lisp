;;; loop.lisp --- Loop-like Macros

;; LOOP extensions

;;; Code:
(in-package :std/macs)

(defkernel loop-kernel (kernel-object)
  ((universe :initform *loop-ansi-universe* :accessor universe))
  (:documentation "A kernel object similar to SB-LOOP::MACRO-STATE for user-defined loops."))
