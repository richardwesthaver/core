;;; var.lisp --- MPK Special Vars

;; 

;;; Code:
(in-package :mpk)

(defvar *mpk-user-directory* (merge-pathnames ".stash/mpk/" (user-homedir-pathname)))

(defvar *mpk-media-directory* "/opt/core/stash/media/")

(defvar *mpk-media-sources* '(:youtube :freesound :spotify :local))
