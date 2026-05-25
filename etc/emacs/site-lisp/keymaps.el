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

(use-package mpc
  :bind
  (:map mpc-mode-map
	("v" . mpc-tagbrowser)
	("a" . mpc-playlist-add)
	("c" . mpc-playlist-create)
	("." . mpc-play-at-point)
	("P" . mpc-resume)
	("f" . mpc-ffwd)
	("b" . mpc-rewind)
	("x" . mpc-playlist-delete)
	("m" . mpc-mark)
	("1" . mpc-playlist))
  :config
  (defun mpc-mark ()
    "Mark mpc song at point and move to next line."
    (interactive)
    (mpc-select-toggle)
    (next-line)))

;;; Modes

(use-package conf-mode
  :bind (:map conf-toml-mode-map
	      ("C-c C-c C-r" . #'rust-run)
	      ("C-c C-c C-u" . #'rust-compile)
	      ("C-c C-c C-t" . #'rust-test)))

(keymap-set emacs-lisp-mode-map "C-c C-l" #'load-file)
(keymap-set emacs-lisp-mode-map "C-c M-k" #'elisp-byte-compile-file)
(keymap-set slime-editing-map "C-c s s" #'slime-sprof-start)
(keymap-set slime-editing-map "C-c s x" #'slime-sprof-stop)
(keymap-set slime-editing-map "C-c s r" #'slime-sprof-report)

(keymap-global-set "C-c (" #'parens-map)
;; (keymap-global-set "C-c )" #'parens-map)

;;; C-x
(keymap-set ctl-x-map "C-b" #'ibuffer)
(keymap-set ctl-x-map "C-M-e" #'eval-last-sexp)

;;; C-x r
(keymap-set ctl-x-r-map "SPC" #'point-to-register)
(keymap-set ctl-x-r-map "C-l" #'list-registers)
(keymap-set ctl-x-r-map "C-b" #'buffer-to-register)
(keymap-set ctl-x-r-map "C-f" #'file-query-to-register)
(keymap-set ctl-x-r-map "C-r" #'copy-register)
;;; C-x x

;;; Global
(keymap-global-set "C-c c" user-map)
(keymap-global-set "<remap> <tab-to-tab-stop>" #'imenu)
(keymap-global-set "<XF86Paste>" parens-map)
(keymap-global-set "C-c C-p" #'outline-previous-heading)
(keymap-global-set "C-c C-n" #'outline-next-heading)
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(keymap-global-set "C-c a" #'org-agenda)
(keymap-global-set "C-c t" #'org-todo)

(provide 'keymaps)
;; keymaps.el ends here
