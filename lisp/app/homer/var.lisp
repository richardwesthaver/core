;;; var.lisp --- Homer Variables

;; 

;;; Code:
(in-package :homer/core)
(defvar *user-homerc* (merge-homedir-pathnames ".homerc"))
(declaim (type home-config *home-config*))
(defvar *home-config*)
(defvar *home-hidden-paths* (nconc *hidden-paths* (list "stash" "store" "readme.org" ".hgignore")))
(defvar *homer-force* nil)
(defvar *homer-logger* nil)
