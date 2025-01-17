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

;;; Downloaders

;; yt
(defun get-playlist ())
(defun get-video ())
(defun get-channel ())
(defun get-media ())
