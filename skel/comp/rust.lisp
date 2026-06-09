;;; skel/comp/rust.lisp --- Rust Components

;; Cargo.toml skel components.

;;; Commentary:

;; (:rust-system "Cargo.toml")

;;; Code:
(in-package :skel/comp/rust)

(defparameter *cargo-manifest-filename* "Cargo.toml")

(defcomponent rust-system (project-module) 
  (config)
  (:keyword :rust-system))

(defcomponent rust-component (project-component)
  (type value)
  (:keyword :rs))

(defmethod print-object ((object rust-system) stream)
  (print-unreadable-object (object stream :type t)
    (format stream ":ID ~A" (format-sxhash (obj/id:id object)))))

(defmethod load-project-component ((kind (eql :rust-system)) (form pathname) &key (path (project-root)))
  (make-instance 'rust-system :config (deserialize (merge-pathnames form path) :toml)))

(defmethod project-compile ((self rust-system) &key &allow-other-keys))

#+todo
(defmethod write-ast ((self rust-system) path))

#+todo
(defmethod read-ast ((self rust-system) path))
