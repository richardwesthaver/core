;;; systemd.lisp --- Systemd CLI Tools

;; 

;;; Code:
(in-package :cli/tools/sys)

(deferror systemd-error (simple-error error) ())

(defun systemd-error (fmt &rest args)
  (error 'systemd-error :format-arguments args :format-control fmt))

(defparameter *systemctl* (find-exe "systemctl"))

(defun run-systemctl (args &key (output t))
  (let ((proc (sb-ext:run-program *systemctl* (or args nil) :output output :wait t)))
    (unless (or (= 0 #1=(sb-ext:process-exit-code proc))
                (= 3 #1#))
      (systemd-error "SYSTEMCTL command failed: ~A ~A" *systemctl* (or args "")))))

(defun systemctl-start (&rest args)
  (run-systemctl (cons "start" args)))

(defun systemctl-stop (&rest args)
  (run-systemctl (cons "stop" args)))

(defun systemctl-status (&rest args)
  (run-systemctl (cons "status" args)))

(defun systemctl-restart (&rest args)
  (run-systemctl (cons "restart" args)))

(defun systemctl-json (&rest args)
  (deserialize
   (with-output-to-string (s)
     (run-systemctl (concatenate 'list '("-q" "-o" "json") args) :output s))
   :json))

;; (systemctl-json "--user")
