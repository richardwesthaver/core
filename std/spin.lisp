;;; spin.lisp --- Spinlocks

;; CAS-based spinlocks using sb-ext:cas and sb-ext:spin-loop-hint

;;; Commentary:

;; based on LPARALLEL.SPIN-QUEUE

;; ref: https://github.com/lmj/lparallel/blob/master/src/spin-queue/cas-spin-queue.lisp

;;; Code:
(in-package :std/spin)
