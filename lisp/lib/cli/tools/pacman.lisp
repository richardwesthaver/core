;;; pacman.lisp --- Pacman Tools

;; Call Pacman from Lisp.

;;; Commentary:

;; Note that many of the pacman commands require sudo.

;;; Code:
(in-package :cli/tools/pacman)

(defun run-pacman (&rest args)
  (apply #'sb-ext:run-program (find-exe "pacman") (or args (list nil))))
