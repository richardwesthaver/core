;;; var.lisp --- MPK Special Vars

;; 

;;; Code:
(in-package :mpk)

(defvar *mpc*)
(defvar *mpk-user-directory* (merge-pathnames ".stash/mpk/" (user-homedir-pathname)))
(defvar *mpk-media-directory* *media-directory*)
(defvar *mpk-data-directory* "/opt/stash/data/mpk/")
(defvar *mpk-cache-directory* "/opt/stash/cache/mpk/")
(defvar *mpk-db-directory* (merge-pathnames "db/" *mpk-data-directory*))
(defvar *mpk-db-meta-directory* (merge-pathnames "meta" *mpk-db-directory*))
(defvar *mpk-db-id-seed* (random 99999))

(defvar *mpk-media-sources* '(:youtube :freesound :spotify :local :torrent))

(defvar *mpk-media-types* '(:audio :video :image :sprite :texture :shader :text))

(defvar *mpk-media-collections*
  (let ((tbl (make-hash-table)))
    (dolist (p '(:music :tv :screenshot :movies :aud :img :vid) tbl)
      (setf (gethash p tbl) (merge-pathnames (string-downcase p) *mpk-media-directory*)))))

;; TODO 2025-03-12: 
(defun find-supported-media-types (type))
  
(defvar *known-media-types*
  (let ((tbl (make-hash-table :size (length *mpk-media-types*))))
    (dolist (m *mpk-media-types* tbl)
      (setf (gethash m tbl) (find-supported-media-types m)))))

(defvar *music-metadata* (make-hash-table))

(defvar *mpk-user-config* nil)

(defvar *user-mpkrc* (merge-homedir-pathnames ".mpkrc"))
