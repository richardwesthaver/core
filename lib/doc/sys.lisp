;;; sys.lisp --- Lisp System Documentation

;; Standard System Documentation.

;;; Code:
(in-package :doc)

(defclass system-documentation ()
  ((system :initarg :system :accessor doc-system :type std:system)))
