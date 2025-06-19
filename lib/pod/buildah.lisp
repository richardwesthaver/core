;;; buildah.lisp --- Buildah Lisp Utils

;; 

;;; Code:
(in-package :pod)

(defvar *buildah-container*)
(defun buildah-from (img)
  (trim 
   (with-output-to-string (s)
     (run-buildah (list "from" img) :output s))))

(defun buildah-add (src dst) 
  (run-buildah 
   (list "add" *buildah-container* (namestring src) (namestring dst)) 
   :output t))

(defun buildah-run (args &key dir env)
  (run-buildah 
   `("run" ,@(when dir `("--workingdir" ,(namestring dir)))
           ,@(when env (flatten
                        (mapcar (lambda (e) 
                                  (if-let ((val (cdr e)))
                                    `("--env" ,(format nil "~A=~A" (car e) val))
                                    `("--unsetenv" ,(car e))))
                                env)))
           ,*buildah-container* ,@(mapcar 'string args))
   :output t))

(defun buildah-copy (&rest args)
  (run-buildah `("copy" ,*buildah-container* ,@args) :output t))

(defun buildah-config (&rest args)
  (run-buildah `("config" ,@args ,*buildah-container*) :output t))

(defmacro with-buildah ((sym from &key commit (rm t)) &body body)
  `(let ((,sym (buildah-from ,from)))
     (setf *buildah-container* ,sym)
     ,@body
     ,@(when commit `((run-buildah (list "commit" ,sym ,commit))))
     ,@(when rm `((run-buildah (list "rm" ,sym))))))

;; (with-buildah (c "o0") (buildah-run '("ls" "-la") :dir "/root/"))
