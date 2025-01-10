;;; systemd.lisp --- Systemd CLI Tools

;; 

;;; Code:
(in-package :cli/tools/systemd)

(deferror systemd-error (simple-error error) ())

(defun systemd-error (fmt &rest args)
  (error 'systemd-error :format-arguments args :format-control fmt))

(defparameter *systemctl* (find-exe "systemctl"))

(defvar *systemctl-output* t)

(defun run-systemctl (&rest args)
  (let ((proc (sb-ext:run-program *systemctl* (or args nil) :output *systemctl-output*)))
    (unless (or (= 0 #1=(sb-ext:process-exit-code proc))
                (= 3 #1#))
      (systemd-error "SYSTEMCTL command failed: ~A ~A" *systemctl* (or args "")))))

(defun systemctl-start (&rest args)
  (apply 'run-systemctl "start" args))

(defun systemctl-stop (&rest args)
  (apply 'run-systemctl "stop" args))

(defun systemctl-status (&rest args)
  (apply 'run-systemctl "status" args))

(defun systemctl-restart (&rest args)
  (apply 'run-systemctl "restart" args))
