;;; vars.lisp --- Packy Variables

;; 

;;; Code:
(in-package :packy/core)

(defparameter *packy-url* (obj/uri:uri "https://packy.compiler.company"))

(defvar *pack* nil)
(defvar *packy-registry* (make-hash-table))
(defvar *packy-compressor* (make-instance 'zstd-compressor))
(defvar *packy-decompressor* (make-instance 'zstd-decompressor))
