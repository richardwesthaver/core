;;; io/ring.lisp --- IO Ring Library

;; 

;;; Code:
(in-package :io/ring)

(load-uring)

(defvar *io* nil)

(defun init-io (&optional (entries 256) (flags 0))
  "Initialize the *IO* variable to an io-uring alien-value type using a
queue size of ENTRIES and settings FLAGS."
  (with-new-io-uring r
    (if (= 0 (io-uring-queue-init entries (addr r) flags))
        (setf *io* r)
        (error "failed to initialize io-uring"))))

;; (defun enter-io (ring))
;; (defun exit-io (ring))
