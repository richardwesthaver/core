;;; lib/skel/comp/cargo.lisp --- Cargo.toml Compiler

;; 

;;; Code:
(in-package :skel/comp/cargo)

(defparameter *default-cargo-manifest* "Cargo.toml")
(defparameter *cargo-manifest-extension* "toml")

(defclass sk-rust-system (sk-module)
  ())

(defclass sk-rust-component (skel)
  (type value))

(defun parse-sk-rust-system (path)
  path)

(defmethod sk-load-component ((kind (eql :rust-system)) (path pathname))
  (declare (ignore kind))
  (parse-sk-rust-system path))

(defmethod sk-compile ((self sk-rust-system) stream &key &allow-other-keys))

(defmethod sk-write-file ((self sk-rust-system) &key path))

(defmethod sk-read-file ((self sk-rust-system) path))
