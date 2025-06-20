;;; virt.lisp --- Container and VM CLI Tools

;; 

;;; Code:
(in-package :cli/tools/virt)

(define-cli-tool :podman (&rest args)
  (let ((proc (sb-ext:run-program *podman* (or args nil) :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (podman-error "PODMAN command failed: ~A ~A" *podman* (or args "")))))

(defun podman-machine-upgrade ()
  (run-podman "machine" "ssh" "sudo rpm-ostree upgrade --check"))

;; podman system service --time=0 unix:///tmp/podman.sock (local-socket)
;; podman system service --time=0 tcp://localhost:8888 (inet-socket :stream :tcp)
(defun start-podman-service (addr &optional (protocol :unix) (time 0))
  "Start the Libpod API on ADDR over PROTO which is either :TCP or :UNIX."
  (declare ((member :unix :tcp) protocol))
  (run-podman "system"
              "service"
              (format nil "~(~a~)://~a" protocol addr)
              (format nil "--time=~a" time)))

(define-cli-tool :buildah (args &key (output t) input)
  (let ((proc (sb-ext:run-program *buildah* args :output output :input input)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (buildah-error "BUILDAH command failed: ~A ~A" *buildah* args))))

(define-cli-tool :mkarchiso (&rest args)
  (let ((proc (sb-ext:run-program *mkarchiso* (or args nil) :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (mkarchiso-error "MKARCHISO command failed: ~A ~A" *mkarchiso* (or args "")))))

(defun find-qemu-exe (&optional (arch (machine-type)))
  (find-exe (concatenate 'string "qemu-system-" (string-downcase (substitute #\_ #\- arch)))))

(defvar *qemu* (find-qemu-exe))

(define-cli-tool :qemu (&rest args)
  (let ((proc (sb-ext:run-program *qemu* (or args nil) :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (qemu-error "QEMU command failed: ~A ~A" *qemu* (or args "")))))
  
(defun run-qemu-img (&rest args)
  (let ((proc (sb-ext:run-program #1=(find-exe "qemu-img") (or args nil) :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (qemu-error "QEMU-IMG command failed: ~A ~A" #1# (or args "")))))
