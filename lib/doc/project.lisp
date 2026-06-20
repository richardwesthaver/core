;;; lib/doc/project.lisp --- Project Documentation

;; Document an entire project.

;;; Commentary:

;;

;;; Code:
(in-package :doc)

(defclass project-documentation (id) ())

(defmethod describe-object ((self project-documentation) stream))
