;;; var.lisp --- MPK Special Vars

;; 

;;; Code:
(in-package :mpk)

(defvar *mpk-user-directory* (merge-pathnames ".stash/mpk/" (user-homedir-pathname)))

(defvar *mpk-media-directory* "/opt/core/stash/media/")

(defvar *mpk-media-sources* '(:youtube :freesound :spotify :local :torrent))

(defvar *mpk-media-types* '(:audio :video :image :sprite :texture :shader :text))

;; TODO 2025-03-12: 
(defun find-supported-media-types (type))
  
(defvar *known-media-types*
  (let ((tbl (make-hash-table :size (length *mpk-media-types*))))
    (dolist (m *mpk-media-types* tbl)
      (setf (gethash m tbl) (find-supported-media-types m)))))
