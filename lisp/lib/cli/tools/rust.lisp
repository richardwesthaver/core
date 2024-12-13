;;; cli/tools/rust.lisp --- Rust Tools

;; 

;;; Code:
(in-package :cli/tools/rust)

(deferror cargo-error (simple-error error) () (:auto t))
(deferror rustup-error (simple-error error) () (:auto t))

(defparameter *cargo* (find-exe "cargo"))
(defparameter *rustup* (find-exe "rustup"))

(defun run-cargo (&rest args)
  (let ((proc (sb-ext:run-program *cargo* (or args nil) :output :stream)))
    (with-open-stream (s (sb-ext:process-output proc))
      (loop for l = (read-line s nil nil)
            while l
            do (write-line l)))
    (if (eq 0 (sb-ext:process-exit-code proc))
        nil
        (cargo-error "CARGO command failed: ~A ~A" *cargo* (or args "")))))

(defun run-rustup (&rest args)
  (let ((proc (sb-ext:run-program *rustup* (or args nil) :output :stream)))
    (with-open-stream (s (sb-ext:process-output proc))
      (loop for l = (read-line s nil nil)
            while l
            do (write-line l)))
    (if (eq 0 (sb-ext:process-exit-code proc))
        nil
        (cargo-error "RUSTUP command failed: ~A ~A" *rustup* (or args "")))))
