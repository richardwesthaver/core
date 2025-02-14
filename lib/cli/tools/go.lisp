;;; go.lisp --- Go Language Tooling

;; 

;;; Code:
(in-package :cli/tools/go)

(define-cli-tool :go (&rest args)
  (let ((proc (sb-ext:run-program *go* (or args nil) :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc)) 
      (go-error "go command failed: ~A ~A" *go* (or args "")))))

(defun go-install (&rest args)
  "Install a go package."
  (apply 'run-go "install" args))
