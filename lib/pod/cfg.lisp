;;; cfg.lisp --- Pod Configs

;; 

;;; Code:
(in-package :pod)

(defconfig podman-config (ast)
  ((machine :initarg :machine)
   (containers :initarg :containers)
   (registries :initarg :registries)
   (storage :initarg :storage)))

(defmethod make-config ((self (eql :podman)) &rest args &key &allow-other-keys)
  (apply 'make-instance 'podman-config args))

(defun load-podman-config (&optional (path *podman-config-directory*))
  (when (probe-file *podman-config-directory*)
    (let ((machine (deserialize (merge-pathnames "podman/machine/qemu/podman-machine-default.json" path) :json))
          (containers (deserialize (merge-pathnames "containers.conf" path) :toml))
          (registries (deserialize (merge-pathnames "registries.conf" path) :toml))
          (storage (deserialize (merge-pathnames "storage.conf" path) :toml)))
      (make-config :podman :containers containers :machine machine :registries registries :storage storage))))

(defun default-podman-config ()
 (load-pod-config))
