;;; lib/skel/comp/cargo.lisp --- Cargo.toml Components

;; Cargo.toml skel components.

;;; Commentary:

;; (:sk-rust-system "Cargo.toml")

;;; Code:
(in-package :skel/comp/cargo)

(defparameter *cargo-manifest-filename* "Cargo.toml")

(defclass sk-rust-system (sk-mod) 
  ((config :initarg :config)))

(defclass sk-rust-component (sk-component)
  (type value))

(defmethod print-object ((object sk-rust-system) stream)
  (print-unreadable-object (object stream :type t)
    (format stream ":ID ~A" (format-sxhash (obj/id:id object)))))

(defmethod sk-load-component ((kind (eql :rust-system)) (form pathname) &optional (path (project-root)))
  (declare (ignore kind))
  (make-instance 'sk-rust-system :config (deserialize (merge-pathnames form path) :toml)))

(defmethod sk-compile ((self sk-rust-system) &key &allow-other-keys))

(defmethod sk-write-file ((self sk-rust-system) &key path)
  (declare (ignorable path)))

(defmethod sk-read-file ((self sk-rust-system) path))
