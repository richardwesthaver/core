;;; lib/obj/color/palette.lisp --- Color Palettes

;; A color palette is a hash-table mapping keywords to RGB instances.

;;; Code:
(in-package :obj/color)

(defvar *x11-color-palette* (let ((tbl (make-hash-table)))
                              (dolist (c *x11-colors* tbl)
                                (setf (gethash (keywordicate (string-trim '(#\+) (symbol-name c))) tbl)
                                      (symbol-value c)))))

(defvar *default-color-palette* *x11-color-palette*)

(defvar *color-palette* *default-color-palette*)

(defun color-palette () *color-palette*)
(defun (setf color-palette) (new) (setf *color-palette* new))
