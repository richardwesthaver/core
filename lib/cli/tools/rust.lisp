;;; cli/tools/rust.lisp --- Rust Tools

;; 

;;; Code:
(in-package :cli/tools/rust)

(define-cli-tool :cargo (&rest args)
  (let ((proc (sb-ext:run-program *cargo* (or args nil) :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (cargo-error "CARGO command failed: ~A ~A" *cargo* (or args "")))))

(define-cli-tool :rustup (&rest args)
  (let ((proc (sb-ext:run-program *rustup* (or args nil) :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (cargo-error "RUSTUP command failed: ~A ~A" *rustup* (or args "")))))

(defun cargo-install (&rest args)
  (apply 'run-cargo "install" args))
