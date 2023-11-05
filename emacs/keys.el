;;; keys.el --- emacs keys -*- lexical-binding: t -*-

;; default keybinds

;;; Commentary:

;; I encourage you to remap these keys as you see fit. Where possible,
;; wrap your custom bindings in a keymap instead of redefining the
;; global defaults defined here.

;;; Code:

;;; User keys
(defvar-keymap toggle-map
  :doc "User-specified keymap for mode toggles. Usually bound to 'C-c c SPC'."
  :prefix 'toggle-map
  "v" #'global-visual-line-mode
  "h" #'global-hl-line-mode
  "l" #'global-line-numbers-mode
  "L" #'global-display-line-numbers-mode
  "a" #'gpm-mouse-mode
  "r" #'refill-mode
  "R" #'global-auto-revert-mode
  "t" #'toggle-frame-tab-bar
  "d" #'toggle-debug-on-error
  "SPC" #'toggle-macro-recording
  "w" #'toggle-theme)

(defvar-keymap status-map
  :doc "User-specified keymap for status functions. Usually bound to 'C-c c .'."
  "l" #'eglot-list-connections
  "p" #'list-processes
  "t" #'list-threads
  "a" #'list-abbrevs
  "c" #'list-timers
  "d" #'list-dynamic-libraries
  "P" #'list-packages)

(defvar-keymap server-map
  :doc "User-specified keymap for server functions. Usually bound to 'C-c c q'."
  "q"  #'kill-emacs
  "r" #'kill-emacs-restart)

(defvar-keymap review-map
  :doc "User-specified keymap for review functions. Usually bound to 'C-c c r'."
  "s" #'flyspell-buffer
  "SPC" #'whitespace-cleanup)

(defvar-keymap user-map
  :doc "User-specified keymap usually bound to 'C-c c' and populated in 'custom.el'."
  :prefix 'user-map
  "c" #'org-capture
  "l" #'org-store-link
  "f" #'load-file
  "u" #'compile
  "a" #'org-agenda
  "<return>" #'shell
  "C-<return>" #'term
  "S-<return>" #'eshell
  "!" #'async-shell-command
  "s" #'speedbar
  "SPC" toggle-map
  "." status-map
  "r" review-map
  "q" server-map
  "<tab>" #'outline-cycle
  "<backtab>" #'outline-cycle-buffer
  "z" #'scratch-new
  "Z" #'default-scratch-buffer
  ";" #'prog-comment-dwim
  "C-;" #'prog-comment-timestamp-keyword)

;;; Modes
(add-hook 
 'conf-toml-mode-hook
 (lambda ()
   (keymap-set conf-toml-mode-map "C-c C-c C-r" #'rust-run)
   (keymap-set conf-toml-mode-map "C-c C-c C-u" #'rust-compile)
   (keymap-set conf-toml-mode-map "C-c C-c C-t" #'rust-test)))

;;; C-x
;; (keymap-set ctl-x-map "C-b" #'ibuffer)

;;; C-x r
(keymap-set ctl-x-r-map "SPC" #'point-to-register)
(keymap-set ctl-x-r-map "C-l" #'list-registers)
(keymap-set ctl-x-r-map "C-b" #'buffer-to-register)
(keymap-set ctl-x-r-map "C-f" #'file-query-to-register)
(keymap-set ctl-x-r-map "C-r" #'copy-register)

;;; C-x x

;;; Global
(keymap-global-set "C-c c" user-map)

(provide 'keys)
;; keys.el ends here
