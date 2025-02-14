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
  ((path :type pathname :initarg :path :accessor path)
   (credentials :type list :initarg :credentials :accessor credentials)))

;; TODO 2024-06-30: 
(defmethod serde ((from authinfo) (to pathname)))
(defmethod serde ((from stream) (to authinfo)))

(defmethod deserialize ((from pathname) (format (eql :authinfo)) &key)
  (make-instance 'authinfo
    :path from
    :credentials
    (remove-if (lambda (x) (char= (char x 0) #\#)) (uiop:read-file-lines from))))

(defmethod deserialize ((from string) (format (eql :authinfo)) &key)
  (make-instance 'authinfo
    :credentials (remove-if (lambda (x) (char= (char x 0) #\#)) (split-sequence #\newline from))))
