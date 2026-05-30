;;; scratch.el --- Scratch files and buffers         -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The Compiler Company

;; Author: Richard Westhaver <richard.westhaver@gmail.com>
;; Keywords: convenience, local

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

;; 

;;; Code:
(defcustom default-scratch-buffer-mode 'lisp-interaction-mode
  "Default major mode for new scratch buffers"
  :group 'default
  :type 'symbol)

;; Adapted from the `scratch.el' package by Ian Eure.
(defun default-scratch-list-modes ()
  "List known major modes."
  (cl-loop for sym the symbols of obarray
	   for name = (symbol-name sym)
	   when (and (functionp sym)
		     (not (member sym minor-mode-list))
		     (string-match "-mode$" name)
		     (not (string-match "--" name)))
	   collect name))

(defun default-scratch-buffer-setup (region &optional mode)
  "Add contents to `scratch' buffer and name it accordingly.

REGION is added to the contents to the new buffer.

Use the current buffer's major mode by default.  With optional
MODE use that major mode instead."
  (let* ((major (or mode major-mode))
	 (string (format "Scratch buffer for: %s\n\n" major))
	 (text (concat string region))
	 (buf (format "*Scratch for %s*" major)))
    (with-current-buffer (get-buffer-create buf)
      (funcall major)
      (save-excursion
	(insert text)
	(goto-char (point-min))
	(comment-region (pos-bol) (pos-eol)))
      (vertical-motion 2))
    (pop-to-buffer buf)))

;;;###autoload
(defun default-scratch-buffer (&optional arg)
  "Produce a bespoke scratch buffer matching current major mode.

With optional ARG as a prefix argument (\\[universal-argument]),
use `default-scratch-buffer-mode'.

With ARG as a double prefix argument, prompt for a major mode
with completion.

If region is active, copy its contents to the new scratch
buffer."
  (interactive "P")
  (let* ((default-mode default-scratch-buffer-mode)
	 (modes (default-scratch-list-modes))
	 (region (with-current-buffer (current-buffer)
		   (if (region-active-p)
		       (buffer-substring-no-properties
			(region-beginning)
			(region-end))
		     "")))
	 (m))
    (pcase (prefix-numeric-value arg)
      (16 (progn
	    (setq m (intern (completing-read "Select major mode: " modes nil t)))
	    (default-scratch-buffer-setup region m)))
      (4 (default-scratch-buffer-setup region default-mode))
      (_ (default-scratch-buffer-setup region)))))

;;;###autoload
(defun scratch-new ()
  "create a new scratch buffer. (could be *scratch* - *scratchN*)"
  (interactive)
  (let ((n 0)
	bufname)
    (while (progn
	     (setq bufname
		   (concat "*scratch"
			   (if (= n 0) "" (int-to-string n))
			   "*"))
	     (setq n (1+ n))
	     (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (insert initial-scratch-message)
    (lisp-interaction-mode)))

(provide 'scratch)
;;; scratch.el ends here
