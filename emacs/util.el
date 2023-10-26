;;; std/util.el --- standard utils  -*- lexical-binding: t -*-

;;; Code:
(require 'cl-lib)

(defmacro when-sys= (name body)
  "(when (string= (system-name) NAME) BODY)"
  `(when ,(string= (system-name) name) ,body))

(defun add-to-load-path (&rest paths)
  "Add PATHS to `load-path'."
  (mapc (lambda (x)
          (cond
           ((listp x) (mapc #'add-to-load-path x))
           ('_ (cl-pushnew x load-path))))
        paths))

(defun join-paths (root &rest dirs)
  "helper function for joining strings to a path."
  (let ((result root))
    (cl-loop for dir in dirs do
             (setq result (concat (file-name-as-directory result) dir)))
    result))

(provide 'init/util)
