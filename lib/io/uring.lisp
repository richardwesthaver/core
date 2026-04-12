;;; io/uring.lisp --- IO_URING High-level Lisp Protocol

;; Drop-in IO_URING API.

;;; Code:
(in-package :io/uring)

(load-uring)

(defvar *io* nil)

;; (defun iou-send (event ring) "Prepare an io-uring submission.")
;; (defun iou-recv (event ring) "Receive an io-uring event.")

(defun setup-uring ())

(defun init-io (&optional (entries 256) (flags 0))
  "Initialize the *IO* variable to an io-uring alien-value type using a
queue size of ENTRIES and settings FLAGS."
  (with-new-io-uring r
    (if (= 0 (uring::io-uring-queue-init entries (addr r) flags))
        (setf *io* r)
        (error "failed to initialize io-uring"))))

;; (defun enter-io (ring))
;; (defun exit-io (ring))
