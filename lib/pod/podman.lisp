;;; lib/pod/podman.lisp --- Pod Manager

;;

;;; Code:
(in-package :pod)

(defvar *podman-exe* (find-exe "podman"))
(defvar *podman-config-directory* (merge-homedir-pathnames ".config/containers/"))
(defvar *buildah-exe* (find-exe "buildah")
  "Path to the 'buildah' executable. This really isn't useful in our environment.")

;; podman system service --time=0 unix:///tmp/podman.sock (local-socket)
;; podman system service --time=0 tcp://localhost:8888 (inet-socket :stream :tcp)
(defun run-podman (&rest args)
  "Run *PODMAN-EXE* using ARGS."
  (sb-ext:run-program *podman-exe* args
                      :input nil
                      :output *trace-output*))

(defun start-podman-service (addr &optional (protocol :unix) (time 0))
  "Start the Libpod API on ADDR over PROTO which is either :TCP or :UNIX."
  (declare ((member :unix :tcp) protocol))
  (run-podman "system"
              "service"
              (format nil "~(~a~)://~a" protocol addr)
              (format nil "--time=~a" time)))
