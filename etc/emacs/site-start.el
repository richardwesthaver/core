;;; site-start.el --- Site Startup -*- lexical-binding: t -*-

;; Core Emacs site startup file.

;;; Commentary:

;; This file is loaded first, before any other file and is responsible
;; for setting up the package manager and load-path, setting
;; site-local variables, loading default.el, then loading all user
;; customizations.

;; init.el -- optional user init file
;; default.el -- site defaults
;; custom.el -- custom file
;; config.el -- user config file
;; USER.el -- user custom file

;;; Code:
(require 'cl-lib)

;;; Variables
(defvar site-lisp-directory "/usr/share/emacs/site-lisp/")
(defvar user-lisp-directory (expand-file-name "lisp" user-emacs-directory))
(defvar user-custom-file (expand-file-name (format "%s.el" user-login-name) user-emacs-directory))
(defvar user-config-file (expand-file-name "config.el" user-emacs-directory))
(defvar user-home-directory (expand-file-name "~"))
(defvar user-lab-directory (expand-file-name "lab" user-home-directory))
(defvar user-stash-directory (expand-file-name ".stash" user-home-directory))
(defvar user-store-directory (expand-file-name ".store" user-home-directory))
(defvar user-mail-directory (expand-file-name "mail" user-home-directory))
(defvar user-org-stash-directory (expand-file-name "org" user-stash-directory))
(defvar default-theme 'modus-vivendi)
(defvar src-directory (expand-file-name "src" user-home-directory))
(defvar company-org-directory (expand-file-name "org" src-directory))
(defvar company-babel-file (expand-file-name "meta/babel.org" company-org-directory))
(defvar company-bibliography (expand-file-name "graph/refs.bib" company-org-directory))
(defvar company-domain "compiler.company")
(defvar company-name "The Compiler Company, LLC")
(defvar company-vc-domain "vc.compiler.company")
(defvar company-url (format "https://%s" company-domain))
(defvar company-vc-url (format "https://%s" company-vc-domain))
(defvar company-packy-domain "packy.compiler.company")
(defvar company-packy-url (format "https://%s" company-packy-domain))
(defvar company-staging-url "the.compiler.company")
(defvar company-cdn-url "https://cdn.compiler.company")
(defvar company-attach-id-dir nil)
(defvar emacs-config-source (expand-file-name "core/emacs" src-directory))
(defvar core-lisp-program "/usr/bin/core")

;;; Keymaps
(defvar-keymap toggle-map
  :doc "User keymap for mode toggles. Usually bound to 'C-c SPC'."
  :prefix 'toggle-map
  "v" 'global-visual-line-mode
  "b" 'speedbar
  "h" 'global-hl-line-mode
  "i" 'imenu-list-smart-toggle
  "N" 'global-display-line-numbers-mode
  "c" 'corfu-mode
  "a" 'gpm-mouse-mode
  "r" 'refill-mode
  "o" 'outline-minor-mode
  "O" 'allout-mode
  "R" 'global-auto-revert-mode
  "t" 'toggle-frame-tab-bar
  "T" 'load-theme
  "d" 'toggle-debug-on-error
  "SPC" 'toggle-macro-recording
  "x" 'org-clock-toggle-auto-clockout
  "s" 'slime-toggle
  "p" 'completion-preview-mode
  "l" 'lisp-toggle
  "w" 'which-key-mode
  "W" 'global-whitespace-toggle-options)

(defvar-keymap status-map
  :doc "User keymap for status functions. Usually bound to 'C-c .'."
  "l" 'eglot-list-connections
  "p" 'list-processes
  "t" 'list-threads
  "a" 'list-abbrevs
  "c" 'list-timers
  "d" 'list-dynamic-libraries
  "v" 'project-vc-dir
  "P" 'list-packages)

(defvar-keymap review-map
  :doc "User keymap for review functions. Usually bound to 'C-c r'."
  "s" 'flyspell-buffer
  "SPC" 'whitespace-cleanup
  "C-s" 'org-schedule-effort
  "u" 'untabify
  "q" 'query-replace-regexp)

(defvar-keymap search-map
  :doc "User keymap for search functions. Usually bound to 'C-c s'."
  "g" 'grep
  "r" 'rgrep
  "z" 'zrgrep
  "f" 're-search-forward
  "b" 're-search-backward
  "d" 'xref-find-definitions
  "a" 'xref-find-apropos
  "SPC" 'whitespace-cleanup)

(defvar-keymap clock-map
  :doc "User keymap for org-clock functions. Usually bound to 'C-c t'."
  "j" 'org-clock-goto
  "i" 'org-clock-in
  "o" 'org-clock-out
  "d" 'org-clock-display
  "x" 'org-clock-cancel
  "l" 'org-clock-in-last
  "e" 'org-clock-modify-effort-estimate
  "m" 'org-clock-mark-default-task)

;;; Settings
(setopt custom-file (expand-file-name "custom.el" user-emacs-directory)
        lisp-directory "/usr/share/emacs/lisp/"
        user-lisp-directory (expand-file-name "lisp" user-emacs-directory)
        tab-width 4
        switch-to-buffer-obey-display-actions t
        show-paren-context-when-offscreen 'overlay
        indent-tabs-mode nil
        make-backup-files nil
        save-list-file-prefix (expand-file-name "auto-save/." user-emacs-directory)
        mml-attach-file-at-the-end t
        confirm-kill-emacs nil
        confirm-kill-processes nil
        use-short-answers t
        display-time-format "%Y-%m-%d %H:%M"
        ring-bell-function 'ignore
        kill-region-dwim nil
        ;; NOTE 2023-11-04: you need to add the following lines to ~/.gnupg/gpg-agent.conf:
        ;; allow-emacs-pinentry
        ;; allow-loopback-pinentry
        epg-pinentry-mode 'loopback
        bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory)
        set-mark-command-repeat-pop t
        tempo-interactive t
        gnus-cache-directory (expand-file-name "gnus" user-emacs-directory)
        url-cache-directory (expand-file-name "url" user-emacs-directory)
        register-use-preview t
        view-read-only t
        ;; inhibit-startup-buffer-menu nil
        ;; initial-buffer-choice t
        scroll-bar-mode nil
        tool-bar-mode nil
        menu-bar-mode nil
        use-dialog-box t
        native-comp-async-report-warnings-errors nil
        comp-deferred-compilation t
        package-native-compile t)

;;; Package Setup
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	    ("nongnu" . "https://elpa.nongnu.org/nongnu/")
	    ("melpa" . "https://melpa.org/packages/")))

;;; Enable Commands
(put 'upcase-region 'disabled nil)
(put 'list-threads 'disabled nil)
(put 'list-timers 'disabled nil)

;;; Global Modes
(kill-ring-deindent-mode t)
(repeat-mode t)
(desktop-save-mode t)

;;; User customizations
(add-hook 'after-init-hook (lambda () (when (file-exists-p user-config-file) (load-file user-config-file))))
(add-hook 'after-init-hook (lambda () (when (file-exists-p user-custom-file) (load-file user-custom-file))))

(provide 'site-start)
;;; site-start.el ends here
