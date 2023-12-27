;;; lib/organ/document.lisp --- Org Document API

;;

;;; Code:
(in-package :organ)

(defclass org-document ()
  ((meta :initform nil :initarg :meta :type (or null org-zeroth-section))
   (tree :initform nil :initarg :tree :type (or (vector org-heading) null))))
