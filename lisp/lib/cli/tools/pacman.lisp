;;; pacman.lisp --- Pacman Tools

;; Call Pacman from Lisp.

;;; Commentary:

;; Note that many of the pacman commands require sudo.

;;; Code:
(in-package :cli/tools/pacman)

(deferror pacman-error (simple-error error) ())

(defun pacman-error (fmt &rest args)
  (error 'pacman-error :format-arguments args :format-control fmt))

(defparameter *pacman* (find-exe "pacman"))

(defun run-pacman (&rest args)
  (let ((proc (sb-ext:run-program *pacman* (or args nil) :output :stream)))
    (with-open-stream (s (sb-ext:process-output proc))
      (loop for l = (read-line s nil nil)
            while l
            do (write-line l)))
    (if (eq 0 (sb-ext:process-exit-code proc))
        nil
        (pacman-error "Pacman command failed: ~A ~A" *pacman* (or args "")))))
