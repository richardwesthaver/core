;;; sys.lisp --- System CLI Tools

;; 

;;; Code:
(in-package :cli/tools/sys)

(deferror systemd-error (simple-error error) ())

(defun systemd-error (fmt &rest args)
  (error 'systemd-error :format-arguments args :format-control fmt))

(defparameter *systemctl* (find-exe "systemctl"))

(defun run-systemctl (args &key (output t))
  (let ((proc (sb-ext:run-program *systemctl* (or args nil) :output output)))
    (unless (or (= 0 #1=(sb-ext:process-exit-code proc))
                (= 3 #1#))
      (systemd-error "SYSTEMCTL command failed: ~A ~A" *systemctl* (or args "")))))

(defun systemctl-start (&rest args)
  (run-systemctl `("start" ,@args)))

(defun systemctl-stop (&rest args)
  (run-systemctl `("stop" ,@args)))

(defun systemctl-status (unit &key (user t) (lines 20))
  (run-systemctl 
   `("status" ,@(when lines `("--lines" ,(format nil "~A" lines)))
              "--no-pager"
              ,@(when user '("--user")) 
              ,unit)))

(defun systemctl-restart (&rest args)
  (run-systemctl `("restart" ,@args)))

(defun systemctl-json (&rest args)
  (deserialize
   (with-output-to-string (s)
     (run-systemctl (concatenate 'list '("-q" "-o" "json") args) :output s))
   :json))

;; (systemctl-json "--user")
