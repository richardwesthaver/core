;;; gen.el --- Emacs support for GEN -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Richard Westhaver

;; Author: Richard Westhaver <richard.westhaver@gmail.com>
;; Keywords: convenience, lisp

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

;; Based on C-MERA's cm-mode.el which searches for a 'cm.indent' file
;; in the current directory/user-emacs-directory and reads it if it
;; exists.

;; In our case we prefer to statically define the supported keywords
;; and indentation as much as possible and providing a configuration
;; API intended for access via SKEL only.

;;; Code:
(defvar gen-keywords
  '(for decl function continue return sizeof typedef void int float
     double long char unsigned signed short auto bool enum struct
     while switch include pragma comment inline const volatile true
     false private protected public class template instantiate
     constructor destructor typename virtual pure cout endl
     using-namespace from-namespace printf fn))

(defvar gen-keywords-rx nil)

;; TODO 2025-10-01: gen.fmt
(define-minor-mode gen-minor-mode
  "Support for SYN/GEN core syntax."
  :lighter " Gen"
  (let ((tail (apply #'concat (mapcar #'(lambda (s) (concat "\\|" (symbol-name s))) (cdr gen-keywords)))))
    (setq gen-keywords-rx (concat "\\<\\(" (symbol-name (car gen-keywords)) tail "\\)\\>"))
    (font-lock-fontify-buffer))
  (message "Gen minor-mode enabled."))

(add-hook 'gen-minor-mode-hook
  (lambda ()
    (when gen-keywords-rx
      (font-lock-add-keywords 
       nil
       `((,gen-keywords-rx . font-lock-keyword-face))))))

;;;###autoload
(defun gen-maybe-enable ()
  (when (and buffer-file-name (equal (file-name-extension buffer-file-name) "gen"))
    (gen-minor-mode 1)))

(provide 'gen)
;;; gen.el ends here
