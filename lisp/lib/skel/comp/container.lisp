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
    (format stream "~A :ID ~A" (file-namestring (containerfile-path object)) (format-sxhash (id object)))))

(defmethod sk-convert ((self containerfile))
  (let ((self (change-class self 'sk-containerfile)))
    (update-id self)
    self))

(defmethod sk-load-component ((kind (eql :containerfile)) (name pathname))
  (declare (ignore kind))
  (sk-convert (deserialize
               (make-pathname :name *default-containerfile* :type (namestring name))
               :containerfile)))

(defmethod sk-write-file ((self sk-containerfile) &key path)
  (serde self (pathname (or path (containerfile-path self)))))
