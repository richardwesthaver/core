;;; box.lisp --- Box Components

;; Box files are currently always archiso configs.

;;; Code:
(in-package :skel/comp/box)

(defclass sk-box-file (sk-component box/archiso:archiso-config) ())

(defmethod sk-convert ((self box-config))
  (let ((ret (change-class self 'sk-box-file)))
    (update-id ret)
    ret))

(defmethod sk-load-component ((kind (eql :box)) form &optional (path (project-root)))
  (declare (ignore kind))
  (sk-convert 
   (config:load-config :archiso 
     (make-pathname :name (namestring form) :type "box" :directory (namestring path)))))

(defmethod sk-build ((self sk-box-file) &key path)
  (build:build self :path path))
                     
(defmethod sk-write-file ((self sk-box-file) &key path)
  (ast:write-ast (ast:build-ast self) (when path (pathname path))))

(defmethod sk-read-file ((self sk-box-file) path)
  (sk-load-component :box path))
