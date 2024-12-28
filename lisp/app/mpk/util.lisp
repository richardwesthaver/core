;;; util.lisp --- MPK Utilities

;; 

;;; Code:
(in-package :mpk)

(defun mpk-path (path)
  (merge-pathnames path (truename *mpk-user-directory*)))

(defun mpk-ensure-directories ()
  (when *mpk-media-directory*
    (ensure-directories-exist *mpk-media-directory*))
  (ensure-directories-exist *mpk-user-directory*))

;;; Metro
