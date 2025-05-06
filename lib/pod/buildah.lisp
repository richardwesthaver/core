;;; buildah.lisp --- Buildah Lisp Utils

;; 

;;; Code:
(in-package :pod)

(defvar *working-container*)
(defun buildah-from (img)
  (trim 
   (with-output-to-string (s)
     (run-buildah (list "from" img) :output s))))

(defun buildah-add (src dst) (run-buildah (list "add" *working-container* src dst) :output t))

(defun buildah-run (args &key dir &allow-other-keys)
  (run-buildah 
   `("run" ,@(when dir `("--workingdir" ,dir)) *working-container* ,@(mapcar 'string args)) 
   :output t))

(defun buildah-config (&rest args)
  (run-buildah `("config" ,@args ,*working-container*) :output t))

(defmacro with-buildah ((sym from &key commit (rm t)) &body body)
  `(let ((,sym (setf *working-container* (buildah-from ,from))))
     ,@body
     ,@(when commit `((run-buildah (list "commit" ,sym ,commit))))
     ,@(when rm `((run-buildah (list "rm" ,sym))))))

;; (with-buildah (c "w0") (run '("ls" "-la") :dir "/root/"))
