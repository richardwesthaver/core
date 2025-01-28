;;; var.lisp --- Homer Variables

;; 

;;; Code:
(in-package :homer/core)

(defparameter *user* (sb-posix:getenv "USER"))
(defparameter *default-user-homerc* (merge-pathnames ".homerc" (user-homedir-pathname)))
(declaim (type home-config *home-config*))
(defvar *home-config*)
(defvar *home-hidden-paths* (nconc *hidden-paths* (list "stash" "store" "readme.org" ".hgignore")))
(defvar *homer-force* nil)
(defvar *homer-logger* nil)
