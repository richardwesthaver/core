;;; lib/obj/color/palette.lisp --- Color Palettes

;; This file defines the COLOR-PALETTE structure object which consists of a
;; hash-table mapping symbol keys to COLOR values. Mappings and palettes
;; themselves are both intended to be implicitly recursive structures.

;;; Code:
(in-package :obj/color)

(defvar *x11-color-palette* (let ((tbl (make-hash-table)))
                              (dolist (c *x11-colors* tbl)
                                (setf (gethash (symbolicate (string-trim '(#\+) (symbol-name c))) tbl)
                                      (symbol-value c)))))

(defvar *default-color-palette* *x11-color-palette*)

(defvar *current-color-palette* *default-color-palette*)

(defstruct color-palette
  (table *default-color-palette* :type hash-table))
