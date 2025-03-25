;;; cfg.lisp --- MPK Config

;; 

;;; Code:
(in-package :mpk)

(defconfig mpk-config () 
  ((mpd :type mpd:mpd-config)))

(defun load-mpkrc ())
