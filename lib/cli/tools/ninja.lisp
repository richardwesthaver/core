;;; ninja.lisp --- Ninja Build Tool

;; 

;;; Code:
(in-package :cli/tools/ninja)

(define-cli-tool :ninja (args &key (wait t) (output t))
  (let ((proc (sb-ext:run-program *ninja* args :wait wait :output output)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (ninja-error "NINJA command failed: ~A ~A" *ninja* (or args "")))))
