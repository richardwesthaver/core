;;; pod.lisp --- Containerfile Components

;; Containerfile skel components.

;;; Commentary:

;; By convention we consider any file with base-name 'Containerfile'
;; (case-sensitive) to be an OCI Containerfile. Extension is used as the name
;; of the containerfile, or if absent defaults to the directory name.

;;; Code:
(in-package :skel/comp/pod)

(defcomponent project-containerfile (project-component containerfile)
  ())

(defmethod print-object ((object project-containerfile) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A" (file-namestring (path object)))))

(defmethod project-convert ((self containerfile))
  (let ((self (change-class self 'project-containerfile)))
    (update-id self)
    self))

(defmethod load-project-component ((kind (eql :containerfile))
                                   name
                                   &key (path (project-root)))
  (project-convert (deserialize
                    (make-pathname :name *default-containerfile* :type (when name (namestring name))
                                   :directory (namestring path))
                    :containerfile)))

(defmethod write-ast ((self project-containerfile) path &key)
  (serde self (pathname (or path (path self)))))

(defmethod read-ast ((self project-containerfile) path)
  (load-project-component :containerfile path))

(defmethod build ((self project-containerfile) &key with-client no-cache tag)
  (typecase with-client
    (null (apply 'pod::run-podman (flatten (concatenate 'list
                                                        `("build" "-f"
                                                          ,(path self)
                                                          ,@(when no-cache (list "--no-cache")))
                                                        (when tag (list "-t" tag ))))))
    ;; iff == t
    (boolean
     (with-libpod-client (c)
       (libpod-request-json c "containers/json")
       (nyi! "need to implement containerfile libpod request method")))
    ;; else
    (t (with-libpod-client (c with-client)
         (nyi! "todo")))))
