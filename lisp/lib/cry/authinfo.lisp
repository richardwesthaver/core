;;; authinfo.lisp --- Gnus Authinfo

;; 

;;; Commentary:

;; ref: https://www.emacswiki.org/emacs/GnusAuthinfo

;;; Code:
(in-package :cry/authinfo)

(defvar *auth-sources* (list #p"~/.authinfo" #p"~/.authinfo.gpg"))

;;; Utils
(defun write-authinfo-line (cons stream)
  (write-string (car cons) stream)
  (write-char #\space stream)
  (write-line (cdr cons) stream))

(defun read-authinfo-line (stream)
  (let ((line (read-line stream nil nil)))
    line))

;;; Obj
(defclass authinfo ()
  ((path :type pathname :initarg :path :accessor authinfo-path)
   (credentials :type list :initarg :credentials :accessor authinfo-credentials)))

;; TODO 2024-06-30: 
(defmethod dat/proto:serde ((from authinfo) (to pathname)))
(defmethod dat/proto:serde ((from stream) (to authinfo)))

(defmethod dat/proto:deserialize ((from pathname) (format (eql :authinfo)) &key))
