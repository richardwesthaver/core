;;; virt.lisp --- Container and VM CLI Tools

;; 

;;; Code:
(in-package :cli/tools/virt)

(define-cli-tool :podman (&rest args)
  (let ((proc (sb-ext:run-program *podman* (or args nil) :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (podman-error "PODMAN command failed: ~A ~A" *podman* (or args "")))))

(define-cli-tool :buildah (args &key output)
  (let ((proc (sb-ext:run-program *buildah* args :output output)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (buildah-error "BUILDAH command failed: ~A ~A" *buildah* args))))
