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
  (apply 'run-podman "exec"
         `(,@(when dir `("-w" ,(namestring dir)))
           ,container
           ,@(if (atom cmd) `(,cmd) cmd))))

(defun podman-save (output &key (container *container*))
  "Save a podman CONTAINER to OUTPUT, which should be a tar file path."
  (apply 'run-podman "save"
         `(,container "-o" ,output)))

(defun podman-import (input)
  (apply 'run-podman "import"
         `(,container ,@(when compress '("-c")) "-o" ,output)))

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
  (apply 'run-podman "run"
         `(,@(when dir `("-w" ,(namestring dir)))
           ,@(when name `("--name" ,name))
           ,@(when tty '("--tty"))
           ,@(when detach '("--detach"))
           ,@(when cmd `("--cmd" ,cmd))
           ,@(when (and name replace) '("--replace"))
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
  (run-podman "stop" container))

(defmacro with-container ((sym container &key run stop name dir tty detach cmd)
                          &body body)
  `(let ((,sym ,(if run
                    `(podman-run ,run 
                                 ,@(when dir `(:dir ,dir))
                                 ,@(when tty `(:tty ,tty))
                                 ,@(when detach `(:detach ,detach))
                                 ,@(when cmd `(:cmd ,cmd))
                                 ,@(when name `(:name ,name))
                                 :container ,container)
                    name)))
     (setq *container* ,sym)
     (unwind-protect (progn ,@body)
       ,@(when stop `((podman-stop ,sym))))))
