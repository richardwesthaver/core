;;; lib/pod/podman.lisp --- Pod Manager

;;

;;; Code:
(in-package :pod)

(defvar *podman-config-directory* (merge-homedir-pathnames ".config/containers/"))

(defvar *container*)

(defun check-container (c)
  (unless c
    (required-argument :container)))

(defun podman-build (&key file tag)
  (apply 'run-podman "build" 
         `(,@(when file `("--file" ,(namestring file)))
           ,@(when tag `("--tag" ,tag)))))

(defun podman-exec (cmd &key dir (container *container*))
  (check-container container)
  (apply 'run-podman "exec"
         `(,@(when dir `("-w" ,(namestring dir)))
           ,container
           ,@(if (atom cmd) `(,cmd) cmd))))

(defun podman-run (args &key dir (container *container*) name (tty t) (detach t))
                             ;; attach cpu 
                             ;; gpu health network 
                             ;; mount memory hostname env
                             ;; dns authfile cap cgroup
                             ;; expose label log mac-address
                             ;; pod publish quiet read-only
                             ;; replace restart requires rm
                             ;; secret systemd timeout tty
                             ;; tz ulimit user volume
  (check-container container)
  (apply 'run-podman "exec"
         `(,@(when dir `("-w" ,(namestring dir)))
           ,@(when name `("--name" ,name))
           ,@(when tty `("--tty" ,name))
           ,@(when detach `("--detach" ,name))
           ,container
           ,@(if (atom args) `(,args) args))))

(defun podman-cp (src dst &key overwrite)
  (apply 'run-podman "cp" `(,@(when overwrite '("--overwrite")) ,(namestring src) ,(namestring dst))))

(defun podman-stop (&optional (container *container*))
  (check-container container)
  (run-podman "stop" container))

(defmacro with-container ((sym container &key run stop name dir tty detach)
                          &body body)
  `(let ((,sym ,(if run 
                    `(podman-run ,run 
                                 ,@(when dir `(:dir ,dir))
                                 ,@(when tty `(:dir ,tty))
                                 ,@(when detach `(:dir ,detach))
                                 ,@(when name `(:name ,name))
                                 :container ,container)
                    container)))
     (setf *container* ,sym)
     ,@body
     ,@(when stop `((podman-stop ,sym)))))

  
