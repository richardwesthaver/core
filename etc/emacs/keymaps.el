;;; keymaps.el --- Default Keymaps -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The Compiler Company

;; Author:  <ellis@zor>
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

;; 

;;; Code:
(defvar-keymap parens-map
  :doc "User keymap for working with parens."
  :repeat (:exit (kill-sexp undo))
  :prefix 'parens-map
  "f" #'forward-sexp
  "b" #'backward-sexp
  "C-n" #'down-list
  "C-p" #'backward-up-list
  "n" #'forward-list
  "p" #'backward-list
  "k" #'kill-sexp
  "\\" #'indent-sexp
  "C-\\" #'prog-indent-sexp
  "/" #'undo
  "t" #'transpose-sexps
  "r" #'raise-sexp
  "c" #'check-parens
  "x" #'eval-defun)

(defvar-keymap toggle-map
  :doc "User keymap for mode toggles. Usually bound to 'C-c c SPC'."
  :prefix 'toggle-map
  "v" #'global-visual-line-mode
  "b" #'speedbar
  "h" #'global-hl-line-mode
  "i" #'imenu-list-smart-toggle
  "N" #'global-display-line-numbers-mode
  "c" #'corfu-mode
  "a" #'gpm-mouse-mode
  "r" #'refill-mode
  "o" #'outline-minor-mode
  "O" #'allout-mode
  "R" #'global-auto-revert-mode
  "t" #'toggle-frame-tab-bar
  "T" #'load-theme
  "d" #'toggle-debug-on-error
  "SPC" #'toggle-macro-recording
  "x" #'org-clock-toggle-auto-clockout
  "s" #'slime-toggle
  "p" #'completion-preview-mode
  "l" #'lisp-toggle
  "w" #'which-key-mode
  "W" #'global-whitespace-toggle-options)

(defvar-keymap status-map
  :doc "User keymap for status functions. Usually bound to 'C-c c .'."
  "l" #'eglot-list-connections
  "p" #'list-processes
  "t" #'list-threads
  "a" #'list-abbrevs
  "c" #'list-timers
  "d" #'list-dynamic-libraries
  "v" #'project-vc-dir
  "P" #'list-packages)

(defvar-keymap server-map
  :doc "User keymap for server functions. Usually bound to 'C-c c q'."
  "q"  #'kill-emacs
  "r" #'kill-emacs-restart)

(defvar-keymap review-map
  :doc "User keymap for review functions. Usually bound to 'C-c c r'."
  "s" #'flyspell-buffer
  "SPC" #'whitespace-cleanup
  "C-s" #'org-schedule-effort
  "u" #'untabify
  "q" #'query-replace-regexp)

(defvar-keymap search-map
  :doc "User keymap for search functions. Usually bound to 'C-c c s'."
  "g" #'grep
  "r" #'rgrep
  "z" #'zrgrep
  "f" #'re-search-forward
  "b" #'re-search-backward
  "d" #'xref-find-definitions
  "a" #'xref-find-apropos
  "SPC" #'whitespace-cleanup)

(defvar-keymap clock-map
  :doc "User keymap for org-clock functions. Usually bound to 'C-c c t'."
  "j" #'org-clock-goto
  "i" #'org-clock-in
  "o" #'org-clock-out
  "d" #'org-clock-display
  "x" #'org-clock-cancel
  "l" #'org-clock-in-last
  "e" #'org-clock-modify-effort-estimate
  "m" #'org-clock-mark-default-task)

(defvar-keymap user-map
  :doc "User keymap. Usually bound to 'C-c c' and populated in 'custom.el'."
  :prefix 'user-map
  "c" #'org-capture
  "1" #'org-inbox-open
  "l" #'org-store-link
  "C-l" #'org-web-tools-insert-link-for-url
  "f" #'load-file
  "u" #'compile
  "a" #'org-agenda
  "A" #'org-agenda-show-week-all
  ;; (keymap-set user-map "<return>" #'eshell)
  "RET" #'eshell
  "C-<return>" #'eshell-new
  "s-<return>" #'term
  "!" #'async-shell-command
  "i" #'imenu
  "SPC" toggle-map
  "." status-map
  "s" search-map
  "v t" #'org-tags-view
  "r" review-map
  "q" server-map
  "(" parens-map
  "M-l" #'duplicate-dwim
  "d i" #'image-dired
  "e p" #'mpc
  "e c" #'edit-emacs-config
  "TAB" #'outline-cycle
  "<backtab>" #'outline-cycle-buffer
  "z" #'scratch-buffer
  "C-z" #'scratch-new
  "Z" #'default-scratch-buffer
  "t" clock-map
  ";" #'prog-comment-dwim
  "C-;" #'prog-comment-timestamp-keyword)

(provide 'keymaps)
;; keymaps.el ends here
