;;; reprex.lisp --- Reproducible Examples

;; Reproducible Examples API

;;; Commentary:

;; [[https://reprex.tidyverse.org/][reprex]]

;; NOTE: reprex should also cover 'evolutions' where the same code is
;; modified, and the differences are highlighted.

;; we provide a generalized version of the original reprex, targeting ULANG
;; org syntax by default.

;; Two formats: single-line and multi-line

;; TODO: markdown (discord,github,etc)

;;; Code:
(in-package :doc)

(deftempo :reprex "")

(deffmt fmt-reprex "")

(defmacro reprex (&body body)
  "Produce a 'reproducible example' from the forms in BODY."
  `(progn ,@body))
