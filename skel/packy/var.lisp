;;; vars.lisp --- Packy Variables

;; 

;;; Code:
(in-package :skel/packy)

(defvar *packy-url* (obj/uri:uri "https://packy.compiler.company"))
(defvar *packy-home* (xdg-data-directory "packy"))
(defvar *pack* nil)
(defvar *packy-registry* (make-hash-table))
;; (defvar *packy-compressor* (make-instance 'zstd-compressor))
;; (defvar *packy-decompressor* (make-instance 'zstd-decompressor))
(defvar *packy-logger* (make-instance 'logger))
(defvar *packy-db* nil)
(defvar *packy-targets* (list *machine-target*))
(defvar *packy-target-table* (make-hash-table)
  "Table mapping MACHINE-TARGETs to TARGET-CONFIGs.")
  
(defparameter *user-packyrc* (xdg-config-file :packy))
(defvar *packy-config*)
