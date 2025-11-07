;;; var.lisp --- Homer Variables

;; 

;;; Code:
(in-package :skel/homer/core)
(declaim (type home-config *home-config*))
(defvar *home-config*)
(defvar *home-hidden-paths* (nconc *hidden-paths* (list "stash" "store" "readme.org" ".hgignore")))
(defvar *homer-force* nil)
(defvar *homer-logger* nil)
(defvar *user-homerc*)
