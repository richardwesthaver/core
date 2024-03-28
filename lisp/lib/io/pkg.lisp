;;; io/pkg.lisp --- high-level IO API

;;

;;; Code:
(defpackage :io
  (:use :cl :std :obj/id :uring :sb-bsd-sockets))

(in-package :io)
                
(defun init-uring (params &optional dontfork)
  "Initialize an IO-URING structure. If NOFORK is non-nil, advise the
kernel that the mmapped regions should not be accessible to forked
processes.")
