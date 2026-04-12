;;; io.lisp --- UIO

;; Linux uio.h

;;; Commentary:

;; This file provides support for IOVECs as used by Linux syscalls and
;; io_uring. IO-VECTORs are based on FOREIGN-VECTORs but may refer to
;; discontinuous slices of memory.

;;; Code:
(in-package :sys)
