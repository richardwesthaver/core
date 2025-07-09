;;; metro.lisp --- MIDI/OSC Client/Server

;; 

;;; Code:
(in-package :mpk/metro)

(defvar *mpk-metro*)
(defvar *mpk-metro-table* (make-hash-table :weakness :value))
(defstruct metro)

(defun mpk-init-metro ()
  (setq *mpk-metro* (make-metro)))

(defconfig metro-config () ())
