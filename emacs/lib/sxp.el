;;; sxp.el --- S-Expression Mode -*- lexical-binding: t; -*-

;; Copyright (C) 2023  ellis

;; Author: ellis <ellis@rwest.io>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs Support for S-Expression files (.sxp).

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Parsing-Expressions.html

;;; Code:
(define-derived-mode sxp-mode lisp-data-mode "S-Expr"
  "Major mode for editing '.sxp' files.")

(add-to-list 'auto-mode-alist '("\\.sxp" . sxp-mode))

;;; OOP
(require 'eieio)

(cl-defgeneric wrap (sxp form))
(cl-defgeneric unwrap (sxp))
(cl-defgeneric from-sxp (sxp form)
  "Update SXP using values from FORM.")
(cl-defgeneric to-sxp (sxp)
  "Return SXP as a list.")
(cl-defgeneric read-sxp (sxp &optional stream)
  "Read S-Expressions directly from STREAM (default =
  `standard-input') and update SXP.")
(cl-defgeneric write-sxp (sxp &optional stream comment)
  "Write S-Expressions directly to STREAM (default =
  `standard-output') with optional COMMENT.")

(defclass sxp ()
  ((form :initarg :form :accessor form)))

(cl-defmethod wrap ((sxp sxp) form)
  (oset sxp :form form))
(cl-defmethod unwrap ((sxp sxp))
  (slot-value sxp 'form))

(cl-defmethod read-sxp ((sxp sxp) &optional stream)
  (initialize-instance sxp (list :form (read stream)))
  sxp)
(cl-defmethod write-sxp ((sxp sxp) &optional stream)
  (print (to-sxp sxp) stream))

;; (defvar sxp-load-tests nil)
;; (when sxp-load-tests
;;   (require 'ert)
;;   (defmacro deftest (name &rest body)
;;     "shorthand for `ert-deftest'."
;;     (declare (indent 1))
;;     `(ert-deftest ,name () :tags '(sxp) ,@body))
;;   (deftest 'sxp:read
;;     (should (read-sxp (sxp) "(hey stranger)")))
;;   (deftest 'sxp:write
;;     (should (write-sxp (sxp :form nil))))
;;   (deftest 'sxp:from
;;     (should (from-sxp (sxp) '(test 1 2 3))))
;;   (deftest 'sxp:to
;;     (should (to-sxp (sxp :form '("test" 'ing)))))
;;   (deftest 'sxp:fmt
;;     (should t))
;;   (deftest 'sxp:mode
;;     (should t)))

(provide 'sxp)
;;; sxp.el ends here
