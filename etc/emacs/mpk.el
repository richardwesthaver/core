;;; mpk.el --- MPK Emacs Support -*- lexical-binding: t; -*-

;; Copyright (C) 2024  The Compiler Company

;; Author: Richard Westhaver <richard.westhaver@gmail.com>
;; Keywords: multimedia

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

;; Commentary:

;; Emacs support for the Media Production Kit (MPK) application.

;;; Code:
(defgroup mpk nil
  "Media Production Kit")

(defcustom mpk-user-directory (expand-file-name "~/media/")
  "MPK user home directory"
  :group 'mpk)

(defcustom mpk-user-lib-directory (join-paths mpk-user-directory "lib/")
  "MPK user library directory."
  :group 'mpk)

(defcustom mpk-save-session-hook nil
  "Hook run after saving mpk session buffer."
  :type 'hook
  :group 'mpk)

(defcustom mpk-open-session-hook nil
  "Hook run after opening mpk session buffer."
  :type 'hook
  :group 'mpk)

(defvar mpk-buffer-name "*mpk*")

(defun mpk-buffer ()
  (or (get-buffer mpk-buffer-name)
      (with-current-buffer (get-buffer-create mpk-buffer-name)
        (setq major-mode 'mpk-mode
              mode-name "MPK"
              mode-line-format (copy-tree mode-line-format))
        (current-buffer))))

(provide 'mpk)
;;; mpk.el ends here
