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

(define-cli-tool :machinectl (&rest args)                                              
  (let ((proc (sb-ext:run-program *machinectl* args :wait t :output t)))               
    (unless (eq 0 (sb-ext:process-exit-code proc))                                    
      (machinectl-error "Machinectl command failed: ~A ~A" *machinectl* (or args "")))))

(define-cli-tool :busctl (&rest args)                                              
  (let ((proc (sb-ext:run-program *busctl* args :wait t :output t)))               
    (unless (eq 0 (sb-ext:process-exit-code proc))                                    
      (busctl-error "Busctl command failed: ~A ~A" *busctl* (or args "")))))

;;; Perf

;; Linux perf is the modern means of collecting performance info. In languages
;; like JS/Java/CL we need to provide symbol info to perf for it to be useful
;; though. The special jit-PID.dump format as well as perf-PID.map files may
;; be generated using SB-PERF - these always go in /tmp/ and are picked up
;; automatically. Once recording with 'perf record -k mono CMD' there is an
;; additional static step needed to fill in the jitdump info: 

;; perf inject -j -i perf.data -o perf.jit.data
(define-cli-tool :perf (cmd args &key (output t) wait)
  (let ((proc (sb-ext:run-program *perf* #1=(cons cmd args) :wait wait :output output)))
    (unless (and wait (eq 0 (sb-ext:process-exit-code proc)))
      (perf-error "PERF command failed: ~A ~A" *perf* #1#))
    proc))

(defun perf-record (&rest args)
  (run-perf "record" args))

(defun perf-inject-jit (&optional (input "perf.data") (output "perf.jit.data"))
  (run-perf "inject" `("-j" "-i" ,input "-o" ,output)))
