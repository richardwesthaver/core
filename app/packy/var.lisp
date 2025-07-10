;;; vars.lisp --- Packy Variables

;; 

;;; Code:
(in-package :packy)

(defvar *packy-url* (obj/uri:uri "https://packy.compiler.company"))
(defvar *packy-home* (merge-pathnames ".stash/packy/" (user-homedir-pathname)))
(defvar *pack* nil)
(defvar *packy-registry* (make-hash-table))
;; (defvar *packy-compressor* (make-instance 'zstd-compressor))
;; (defvar *packy-decompressor* (make-instance 'zstd-decompressor))
(defvar *packy-logger* (make-instance 'logger))
(defvar *packy-db* nil)
(defparameter *default-packy-dist-targets* '("aarch64-unknown-linux-gnu" "x86_64-unknown-linux-gnu" "x86_64-unknown-linux-musl"))
(defvar *packy-dist-targets* *default-packy-dist-targets*)
