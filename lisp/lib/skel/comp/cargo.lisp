;;; lib/skel/comp/cargo.lisp --- Cargo.toml Components

;; Cargo.toml skel components.

;;; Commentary:

;; (:sk-rust-system "Cargo.toml")

;;; Code:
(in-package :skel/comp/cargo)

(defparameter *default-cargo-manifest* "Cargo.toml")
(defparameter *cargo-manifest-extension* "toml")

(defclass sk-rust-system (sk-mod)
  ())

(defclass sk-rust-component (sk-component)
  (type value))

(defmethod print-object ((object sk-rust-system) stream)
  (print-unreadable-object (object stream :type t)
    (format stream ":ID ~A" (format-sxhash (obj/id:id object)))))

(defun parse-sk-rust-system (path)
  path)

(defmethod sk-load-component ((kind (eql :rust-system)) (form pathname) &optional (path *default-pathname-defaults*))
  (declare (ignore kind))
  (parse-sk-rust-system (merge-pathnames form path)))

(defmethod sk-compile ((self sk-rust-system) &key &allow-other-keys))

(defmethod sk-write-file ((self sk-rust-system) &key path))

(defmethod sk-read-file ((self sk-rust-system) path))
