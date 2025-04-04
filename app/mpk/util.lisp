;;; util.lisp --- MPK Utilities

;; 

;;; Code:
(in-package :mpk)

(defun mpk-media-collection (k)
  (gethash k *mpk-media-collections*))

(defun mpk-user-path (path)
  (merge-pathnames path *mpk-user-directory*))

(defun mpk-media-path (path)
  (merge-pathnames path *mpk-media-directory*))

(defun mpk-music-path (path)
  (merge-pathnames path (mpk-media-collection :music)))

(defun mpk-data-path (path)
  (merge-pathnames path *mpk-data-directory*))

(defun mpk-ensure-directories ()
  (maphash-values (lambda (p) (ensure-directories-exist p :verbose t)) *mpk-media-collections*)
  (ensure-directories-exist *mpk-user-directory* :verbose t))
