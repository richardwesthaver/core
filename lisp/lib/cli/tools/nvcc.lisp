;;; nvcc.lisp --- NVCC Support

;; 

;;; Code:
(in-package :cli/tools/nvcc)

(deferror nvcc-error (simple-error) () (:auto t))
(defparameter *nvcc* (find-exe "nvcc"))
(defun run-nvcc (&rest args)
  (let ((proc (sb-ext:run-program *nvcc* (or args nil) :output :stream)))
    (with-open-stream (s (sb-ext:process-output proc))
      (loop for l = (read-line s nil nil)
            while l
            do (write-line l)))
    (if (eq 0 (sb-ext:process-exit-code proc))
        nil
        (nvcc-error "NVCC command failed: ~A ~@[~A~]" *nvcc* args))))
