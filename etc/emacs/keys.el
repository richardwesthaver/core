;;; keys.el --- emacs keys -*- lexical-binding: t; -*-

;; default keybinds

;;; Commentary:

;; I encourage you to remap these keys as you see fit. Where possible,
;; wrap your custom bindings in a keymap instead of redefining the
;; global defaults defined here.

;;; Code:
(require 'default)

(defvar-keymap parens-map
  :doc "parens-minor-mode keymap."
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
  :doc
  "User-specified keymap for mode toggles. Usually bound to 'C-c c SPC'."
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
  :doc
  "User-specified keymap for status functions. Usually bound to 'C-c c .'."
  "l" #'eglot-list-connections
  "p" #'list-processes
  "t" #'list-threads
  "a" #'list-abbrevs
  "c" #'list-timers
  "d" #'list-dynamic-libraries
  "v" #'project-vc-dir
  "P" #'list-packages)

(defvar-keymap server-map
  :doc
  "User-specified keymap for server functions. Usually bound to 'C-c c q'."
  "q"  #'kill-emacs
  "r" #'kill-emacs-restart)

(defvar-keymap review-map
  :doc
  "User-specified keymap for review functions. Usually bound to 'C-c c r'."
  "s" #'flyspell-buffer
  "SPC" #'whitespace-cleanup
  "C-s" #'org-schedule-effort
  "u" #'untabify
  "q" #'query-replace-regexp)

(defvar-keymap search-map
  :doc
  "User-specified keymap for search functions. Usually bound to 'C-c c s'."
  "g" #'grep
  "r" #'rgrep
  "z" #'zrgrep
  "f" #'re-search-forward
  "b" #'re-search-backward
  "d" #'xref-find-definitions
  "a" #'xref-find-apropos
  "SPC" #'whitespace-cleanup)

(defvar-keymap clock-map
  :doc
  "User-specified keymap for org-clock functions. Usually bound to 'C-c c t'."
  "j" #'org-clock-goto
  "i" #'org-clock-in
  "o" #'org-clock-out
  "d" #'org-clock-display
  "x" #'org-clock-cancel
  "l" #'org-clock-in-last
  "e" #'org-clock-modify-effort-estimate
  "m" #'org-clock-mark-default-task)

(defvar-keymap user-map
  :doc
  "User-specified keymap usually bound to 'C-c c' and populated in 'custom.el'."
  :prefix 'user-map
  "c" #'org-capture
  "1" #'org-inbox-open
  "l" #'org-store-link
  "C-l" #'org-web-tools-insert-link-for-url
  "f" #'load-file
  "u" #'compile
  "a" #'org-agenda
  "A" #'org-agenda-show-week-all
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

;;;_. MPC
(defun mpc-mark ()
  "Mark mpc song at point and move to next line."
  (interactive)
  (mpc-select-toggle)
  (next-line))

(with-eval-after-load "mpc"
  (keymap-set mpc-mode-map "v" 'mpc-tagbrowser)
  (keymap-set mpc-mode-map "a" 'mpc-playlist-add)
  (keymap-set mpc-mode-map "c" 'mpc-playlist-create)
  (keymap-set mpc-mode-map "." 'mpc-play-at-point)
  (keymap-set mpc-mode-map "P" 'mpc-resume)
  (keymap-set mpc-mode-map "f" 'mpc-ffwd)
  (keymap-set mpc-mode-map "b" 'mpc-rewind)
  (keymap-set mpc-mode-map "x" 'mpc-playlist-delete)
  (keymap-set mpc-mode-map "m" 'mpc-mark)
  (keymap-set mpc-mode-map "1" 'mpc-playlist))

;;; Modes
(add-hook
 'conf-toml-mode-hook
 (lambda ()
   (keymap-set conf-toml-mode-map "C-c C-c C-r" #'rust-run)
   (keymap-set conf-toml-mode-map "C-c C-c C-u" #'rust-compile)
   (keymap-set conf-toml-mode-map "C-c C-c C-t" #'rust-test)))

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

(provide 'keys)
;; keys.el ends here
