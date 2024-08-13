;;; cargo.lisp --- Cargo Tools

;; 

;;; Code:
(in-package :cli/tools/cargo)

(deferror cargo-error (simple-error error) ())

(defun cargo-error (fmt &rest args)
  (error 'cargo-error :format-arguments args :format-control fmt))

(defparameter *cargo* (find-exe "cargo"))

(defun run-cargo (&rest args)
  (let ((proc (sb-ext:run-program *cargo* (or args nil) :output :stream)))
    (with-open-stream (s (sb-ext:process-output proc))
      (loop for l = (read-line s nil nil)
            while l
            do (write-line l)))
    (if (eq 0 (sb-ext:process-exit-code proc))
        nil
        (cargo-error "CARGO command failed: ~A ~A" *cargo* (or args "")))))
