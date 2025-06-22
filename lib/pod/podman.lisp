;;; lib/pod/podman.lisp --- Pod Manager

;;

;;; Code:
(in-package :pod)

(defvar *podman-config-directory* (merge-homedir-pathnames ".config/containers/"))

(defvar *container* nil)

(defun check-container (&optional (c *container*))
  (unless c
    (required-argument :container)))

(defun podman-build (&key file tag no-cache)
  (apply 'run-podman "build" 
         `(,@(when file `("--file" ,(namestring file)))
           ,@(when tag `("--tag" ,tag))
           ,@(when no-cache `("--no-cache")))))

(defun podman-exec (cmd &key dir (container *container*))
  (check-container container)
  (apply 'run-podman "exec"
         `(,@(when dir `("-w" ,(namestring dir)))
           ,container
           ,@(if (atom cmd) `(,cmd) cmd))))

(defun podman-run (args &key dir (container *container*) name (tty t) (detach t) cmd (replace t) systemd ports)
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
           ,@(when tty `("--tty" ,tty))
           ,@(when detach `("--detach" ,detach))
           ,@(when cmd `("--cmd" ,cmd))
           ,@(when replace '("--replace"))
           ,@(when ports (flatten
                          (mapcar 
                           (lambda (x) 
                             (list "-p" 
                                   (if (consp x) 
                                       (format nil "~A:~A" (car x) (cdr x))
                                       x)))
                           ports)))
           ,@(when systemd '("--systemd=true"))
           ,container
           ,@(if (atom args) `(,args) args))))

(defun podman-cp (src dst &key overwrite)
  (apply 'run-podman "cp" `(,@(when overwrite '("--overwrite")) ,(namestring src) ,(namestring dst))))

(defun podman-stop (&optional (container *container*))
  (check-container container)
  (run-podman "stop" container))

(defmacro with-container ((sym container &key run stop name dir tty detach cmd)
                          &body body)
  `(let ((,sym ,(if run 
                    `(podman-run ,run 
                                 ,@(when dir `(:dir ,dir))
                                 ,@(when tty `(:dir ,tty))
                                 ,@(when detach `(:dir ,detach))
                                 ,@(when cmd `(:cmd ,cmd))
                                 ,@(when name `(:name ,name))
                                 :container ,container)
                    container)))
     (setf *container* ,sym)
     ,@body
     ,@(when stop `((podman-stop ,sym)))))
