;;; box.lisp --- Box Components

;;

;;; Code:
(in-package :skel/comp/box)

(defclass sk-box-file (sk-component box-config) ())
(defclass sk-archiso-file (sk-box-file archiso-config) ())
(defclass sk-qemu-image-file (sk-box-file qemu-image-config) ())

(defmethods sk-convert 
  (((self box-config))
   (let ((ret (change-class self 'sk-box-file)))
     (update-id ret)
     ret))
  (((self archiso-config))
   (let ((ret (change-class self 'sk-archiso-file)))
     (update-id ret)
     ret))
  (((self qemu-image-config))
   (let ((ret (change-class self 'sk-archiso-file)))
     (update-id ret)
     ret)))

(defmethod sk-load-component ((kind (eql :box)) form &optional (path (project-root)))
  (declare (ignore kind))
  (let* ((n (pathname-name form))
         (p (make-pathname :name n :type "box" :directory (namestring path)))
         (ret (sk-convert (config:load-config :box p))))
    (setf (name ret) n
          (path ret) p)
    ret))

(defmethod sk-build ((self sk-box-file) &key (path (find-stash-directory)))
  (build self :path path))
                     
(defmethod sk-write-file ((self sk-box-file) &key (path (stash-pathname (name self))))
  (ast:write-ast (build self) (when path (pathname path))))

(defmethod sk-read-file ((self sk-box-file) path)
  (sk-load-component :box path))
