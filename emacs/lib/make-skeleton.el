;;; make-skeleton.el --- Major mode for making skeletons -*- lexical-binding: t; -*-

;; Copyright (C) 2023 ellis

;; Author: ellis <ellis@rwest.io>
;; Keywords: convenience

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

;; Copied from http://www.panix.com/~tehom/my-code/skel-recipe.txt

;;; Code:
(defvar make-skeleton-saved-winconf nil)
(defvar make-skeleton-header ";; make-skeleton-mode
;; (find-w3m \"http://www.panix.com/~tehom/my-code/skel-recipe.txt\")
;; (describe-function 'skeleton-insert)
;; These lines are ignored.
"
  "Header string for skeleton.")

(defun make-skeleton ()
  "Create skeleton of skeleton.
It is based on `A recipe for using skeleton.el'.
http://www.panix.com/~tehom/my-code/skel-recipe.txt

C-c C-e: Erase the skeleton contents.
C-c C-c: Finish the input."
  (interactive)
  (setq make-skeleton-saved-winconf (current-window-configuration))
  (switch-to-buffer "*make-skeleton*")
  (make-skeleton-mode)
  (if (zerop (buffer-size))
      (make-skeleton-erase-buffer)))

(defun make-skeleton-finish ()
  (interactive)
  (set-window-configuration (or make-skeleton-saved-winconf (current-window-configuration)))
  (insert "\n(define-skeleton ")
  (save-excursion
    (insert "_\n"
            "\"Insert _\" nil\n")
    (let ((lines (with-current-buffer (get-buffer-create "*make-skeleton*")
                   ;; skip header
                   (goto-char (point-min))
                   (re-search-forward "^[;]")
                   (beginning-of-line)
                   (split-string (buffer-substring (point) (point-max)) "\n"))))
      (dolist (line lines nil)
        (back-to-indentation)
        (insert (format "%S > \\n\n" line))))
    (insert ")\n")))

(defun make-skeleton-erase-buffer ()
  "Erase the skeleton contents."
  (interactive)
  (erase-buffer)
  (insert make-skeleton-header))


(define-derived-mode make-skeleton-mode fundamental-mode "skeleton"
  "Major mode for creating a skeleton of skeleton."
  (define-key make-skeleton-mode-map "\C-c\C-c" 'make-skeleton-finish)
  (define-key make-skeleton-mode-map "\C-c\C-e" 'make-skeleton-erase-buffer))

(provide 'make-skeleton)
;;; make-skeleton.el ends here
