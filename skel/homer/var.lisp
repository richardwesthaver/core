;;; var.lisp --- Homer Variables

;; 

;;; Code:
(in-package :skel/homer/core)
(defvar *home-config* nil)
(defvar *home-hidden-paths* (nconc *hidden-paths* (list "stash" "store" "readme.org" ".hgignore")))
(defvar *homer-force* nil)
(defvar *homer-logger* nil)
(defvar *user-homerc* (xdg-config-file :home))
