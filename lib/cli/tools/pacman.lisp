;;; pacman.lisp --- Pacman Tools

;; Call Pacman from Lisp.

;;; Commentary:

;; Note that many of the pacman commands require sudo.

;;; Code:
(in-package :cli/tools/pacman)

(define-cli-tool :pacman (&rest args)
  (let ((proc (sb-ext:run-program *pacman* (or args nil) :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (pacman-error "Pacman command failed: ~A ~A" *pacman* (sb-ext:process-error proc)))))
