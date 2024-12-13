;;; util.lisp --- MPK Utilities

;; 

;;; Code:
(in-package :mpk/int)

(defun mpk-path (path)
  (merge-pathnames path *mpk-directory*))
