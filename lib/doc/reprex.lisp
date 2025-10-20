;;; reprex.lisp --- Reproducible Examples

;; Reproducible Examples API

;;; Commentary:

;; ref: https://reprex.tidyverse.org/

;;; Code:
(in-package :doc)

(defvar *reprex-format*)

(defmacro reprex (&body body)
  "Produce a 'reproducible example' from the forms in BODY."
  `(progn ,@body))
