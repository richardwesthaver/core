;;; var.lisp --- Additional StumpWM Vars

;; 

;;; Code:
(in-package :x11/stump)
(defvar *default-stumpwm-modules* nil)
(defvar *default-stumpwm-prefix-key* (kbd "s-SPC"))
(defvar *user-map* (make-sparse-keymap))
(defvar *sudo-map* (make-sparse-keymap))
(defvar *nav-map* (make-sparse-keymap))
(defvar *toggle-map* (make-sparse-keymap))
(defvar *edit-map* (make-sparse-keymap))
(defvar *app-map* (make-sparse-keymap))
