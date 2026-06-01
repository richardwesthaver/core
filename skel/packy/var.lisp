;;; vars.lisp --- Packy Variables

;; 

;;; Code:
(in-package :skel/packy)

(defvar *packy-url* (obj/uri:uri "https://packy.compiler.company"))
(defvar *packy-home* #p"/srv/pkg/"
 "Default home directory of the PACKY system. This directory is used for package
output and is often accessible over WAN.")

(defvar *pack* nil "The current PACK, if any.")
  
(defvar *packy-registry* (make-hash-table)
  "Local cache mapping keys to PACK instances.")
;; (defvar *packy-compressor* (make-instance 'zstd-compressor))
;; (defvar *packy-decompressor* (make-instance 'zstd-decompressor))
(defvar *packy-logger* (make-instance 'logger))
(defvar *packy-db* nil)
(defvar *packy-targets* (list *machine-target*))
(defvar *packy-target-table* (make-hash-table)
  "Table mapping MACHINE-TARGETs to TARGET-CONFIGs.")
  
(defparameter *user-packyrc* (xdg-config-file :packy))
(defparameter *system-packyrc* (pathname "/etc/packyrc"))

(defvar *packy-config*)

(defparameter *default-pkg-extension* "pkg")

(defvar *default-pkgfile* "pkgfile")
