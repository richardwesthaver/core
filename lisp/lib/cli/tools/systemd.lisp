;;; systemd.lisp --- Systemd CLI Tools

;; 

;;; Code:
(in-package :cli/tools/systemd)

(deferror systemd-error (simple-error error) ())

(defun systemd-error (fmt &rest args)
  (error 'systemd-error :format-arguments args :format-control fmt))

(defparameter *systemctl* (find-exe "systemctl"))

(defun run-systemctl (&rest args)
  (let ((proc (sb-ext:run-program *systemctl* (or args nil) :output :stream)))
    (with-open-stream (s (sb-ext:process-output proc))
      (loop for l = (read-line s nil nil)
            while l
            do (write-line l)))
    (if (eq 0 (sb-ext:process-exit-code proc))
        nil
        (systemd-error "SYSTEMCTL command failed: ~A ~A" *systemctl* (or args "")))))
