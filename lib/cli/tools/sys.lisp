;;; sys.lisp --- System CLI Tools

;; 

;;; Code:
(in-package :cli/tools/sys)

(define-cli-tool :systemctl)

(defun run-systemctl (args &key (output t))
  (let ((proc (sb-ext:run-program *systemctl* (or args nil) :output output)))
    (unless (or (= 0 #1=(sb-ext:process-exit-code proc))
                (= 3 #1#))
      (systemctl-error "SYSTEMCTL command failed: ~A ~A" *systemctl* (or args "")))))

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
(defclass! systemd-unit (ini-document)
  ((name :accessor name)
   type))

(defmethod deserialize ((from pathname) (format (eql :systemd-unit)) &rest args)
  (let ((typ (pathname-type from))
        (name (pathname-name from))
        (ret (change-class (apply 'deserialize from :ini args) 'systemd-unit)))
    (setf (slot-value ret 'type) typ
          (name ret) name)
    ret))
             
(defun parse-json-unit (json)
  (let ((ast (ast:ast json)))
    (assert (string-equal "unit" (caar ast)))
    (let* ((name (cadr (pop ast)))
           (type (pathname-type name)))
      (setf name (pathname-name name))
      (flet ((.assoc (x) (when-let ((y (assoc x ast :test 'string-equal))) (cadr y))))
        `(:type ,type
          :name ,name 
          :load ,(.assoc "load") 
          :active ,(.assoc "active") 
          :sub ,(.assoc "sub") 
          :description ,(.assoc "description"))))))

(defun systemd-units (&optional user)
  (mapcar 'parse-json-unit (apply 'systemctl-json (when user (list "--user")))))

(define-cli-tool :journalctl (&rest args)
  (let ((proc (sb-ext:run-program *journalctl* args :wait t :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (journalctl-error "Journalctl command failed: ~A ~A" *journalctl* (or args "")))))

(define-cli-tool :networkctl (&rest args)
  (let ((proc (sb-ext:run-program *networkctl* args :wait t :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (networkctl-error "Networkctl command failed: ~A ~A" *networkctl* (or args "")))))

(define-cli-tool :resolvectl (&rest args)
  (let ((proc (sb-ext:run-program *networkctl* args :wait t :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (resolvectl-error "Networkctl command failed: ~A ~A" *resolvectl* (or args "")))))

(define-cli-tool :loginctl (&rest args)
  (let ((proc (sb-ext:run-program *loginctl* args :wait t :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (resolvectl-error "Loginctl command failed: ~A ~A" *loginctl* (or args "")))))

(define-cli-tool :homectl (&rest args)
  (let ((proc (sb-ext:run-program *homectl* args :wait t :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (resolvectl-error "Homectl command failed: ~A ~A" *homectl* (or args "")))))

(define-cli-tool :userdbctl (&rest args)
  (let ((proc (sb-ext:run-program *userdbctl* args :wait t :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (resolvectl-error "Userdbctl command failed: ~A ~A" *userdbctl* (or args "")))))
