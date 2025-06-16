;;; container.lisp --- Containerfile Components

;; Containerfile skel components.

;;; Commentary:

;; By convention we consider any file with base-name 'Containerfile'
;; (case-sensitive) to be an OCI Containerfile. Extension is used as the name
;; of the containerfile, or if absent defaults to the directory name.

;;; Code:
(in-package :skel/comp/container)

(defclass sk-containerfile (sk-component containerfile)
  ())

(defmethod print-object ((object sk-containerfile) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A" (file-namestring (path object)))))

(defmethod sk-convert ((self containerfile))
  (let ((self (change-class self 'sk-containerfile)))
    (update-id self)
    self))

(defmethod sk-load-component ((kind (eql :containerfile))
                              (name pathname)
                              &optional (path (project-root)))
  (declare (ignore kind))
  (sk-convert (deserialize
               (make-pathname :name *default-containerfile* :type (namestring name)
                              :directory (namestring path))
               :containerfile)))

(defmethod sk-write-file ((self sk-containerfile) &key path)
  (serde self (pathname (or path (path self)))))

(defmethod sk-read-file ((self sk-containerfile) path)
  (sk-load-component :containerfile path))

(defmethod sk-build ((self sk-containerfile) &key with-client no-cache tag)
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
