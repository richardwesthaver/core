;;; box.lisp --- Box Components

;;

;;; Code:
(in-package :skel/comp/box)

(defclass box-file (project-component box-config) ())
(defclass archiso-file (box-file archiso-config) ())
(defclass qemu-image-file (box-file qemu-image-config) ())
(defclass qemu-system-file (box-file qemu-system-config) ())

(defmethods project-convert 
  (((self box-config))
   (let ((ret (change-class self 'box-file)))
     (update-id ret)
     ret))
  (((self archiso-config))
   (let ((ret (change-class self 'archiso-file)))
     (update-id ret)
     ret))
  (((self qemu-image-config))
   (let ((ret (change-class self 'qemu-image-file)))
     (update-id ret)
     ret))
  (((self qemu-system-config))
   (let ((ret (change-class self 'qemu-system-file)))
     (update-id ret)
     ret)))

(defmethod load-project-component ((kind (eql :box)) form &key (path (project-root)))
  (declare (ignore kind))
  (let* ((n (pathname-name form))
         (p (make-pathname :name n :type "box" :directory (namestring path)))
         (ret (project-convert (config:load-config :box p))))
    (setf (name ret) n
          (path ret) p)
    ret))

(defmethod build ((self box-file) &key (path (find-stash-directory)))
  (build self :path path))
                     
(defmethod write-ast ((self box-file) stream &key (path (stash-pathname (name self))))
  (ast:write-ast (build self) stream :path (when path (pathname path))))

(defmethod read-ast ((self box-file) path)
  (load-project-component :box path))
