;;; go.lisp --- Go Language Tooling

;; 

;;; Code:
(in-package :cli/tools/go)

(deferror go-error (simple-error error) () (:auto t))

(defparameter *go* (find-exe "go"))

(defun run-go (&rest args)
  (let ((proc (sb-ext:run-program *go* (or args nil) :output t)))
    (if (eq 0 (sb-ext:process-exit-code proc))
        nil
        (go-error "go command failed: ~A ~A" *go* (or args "")))))

(defun go-install (&rest args)
  "Install a go package."
  (apply 'run-go "install" args))
