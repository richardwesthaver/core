;;; io/pkg.lisp --- high-level IO API

;;

;;; Commentary:

;; pay close attention to the spec for opportunities to replace io
;; primitives -- for example WITH-OPEN-FILE accepts a :CLASS keyword
;; argument, which defaults to SB-SYS:FD-STREAM.

;; this package would be responsible for providing an alternative
;; class, something like IO-STREAM.

;;; Code:
(defpackage :io
  (:use :cl :std :obj/id :uring :sb-bsd-sockets)
  (:import-from :sb-alien :addr)
  (:import-from :uring :build)
  (:shadowing-import-from :uring :load-uring)
  (:export :load-uring :*io*
   :init-io :enter-io :exit-io))

(in-package :io)

(load-uring)

(defvar *io* nil)

(defun init-io (&optional (entries 256) (flags 0))
  "Initialize the *IO* variable to an io-uring alien-value type using a
queue size of ENTRIES and settings FLAGS."
  (with-new-io-uring r
    (if (= 0 (io-uring-queue-init entries (addr r) flags))
        (setf *io* r)
        (error "failed to initialize io-uring"))))

(defun enter-io (ring))
(defun exit-io (ring))
