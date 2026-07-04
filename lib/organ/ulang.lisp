;;; ulang.lisp --- ulang compliance lib

;; ULANG support for Lisp

;;; Commentary:

;; see also [[vc:core:etc/emacs/ulang.el][ulang.el]]

;; This file defines generic counterparts to the ulang elisp package.

;;; Code:
(in-package :organ)

(defvar *ulang-todo-keywords* '("TODO" "REVIEW" "FIX" "HACK" "RESEARCH" "NOTE"))

(defvar ulang-comment-rx (create-scanner "^;+ (\\w+)\\s?(.*):\\s(.*)$"))

(defun parse-ulang-comment (line)
  (coerce (nth-value 1 (scan-to-strings ulang-comment-rx line)) 'list))
