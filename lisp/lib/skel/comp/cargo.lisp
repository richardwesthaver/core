;;; lib/skel/comp/cargo.lisp --- Cargo.toml Compiler

;; 

;;; Code:
(in-package :skel/comp/cargo)

(defparameter *default-cargo-manifest* "Cargo.toml")
(defparameter *cargo-manifest-extension* "toml")

(defclass sk-rust-system (skel sk-meta)
  ())

(defclass sk-rust-component (skel)
  (type value))

(defmethod sk-compile ((self sk-rust-system) stream &key &allow-other-keys))

(defmethod sk-write-file ((self sk-rust-system) &key path))

(defmethod sk-read-file ((self sk-rust-system) path))
