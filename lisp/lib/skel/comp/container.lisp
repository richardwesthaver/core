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

(defmethod sk-convert ((self containerfile))
  (let ((self (change-class self 'sk-containerfile)))
    (update-id self)
    self))

(defmethod sk-load-component ((kind (eql :containerfile)) (name pathname))
  (declare (ignore kind))
  (sk-convert (deserialize
               (make-pathname :name *default-containerfile* :type (namestring name))
               :containerfile)))

(defmethod sk-read-file ((self sk-containerfile) (path pathname)))
(defmethod sk-write-file ((self sk-containerfile) &key path))
