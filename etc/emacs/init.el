;;; init.el --- emacs init -*- lexical-binding: t -*-

;; Core Emacs init file.

;;; Commentary:

;; This file is loaded immediately after early-init.el and is
;; responsible for setting up the package manager and load-path,
;; setting site-local variables, loading default.el, then loading all
;; user customizations:

;; 

;;; Code:
;;; Utils
(defun add-to-load-path (&rest paths)
  "Add PATHS to `load-path'."
  (mapcar (lambda (x) (add-to-list 'load-path x)) paths))

(defun darwin-p () (string= system-type "darwin"))
(defun linux-p () (string= system-type "gnu/linux"))

(defun join-paths (root &rest dirs)
  "helper function for joining strings to a path."
  (let ((result root))
    (cl-loop for dir in dirs do
             (setq result (concat (file-name-as-directory result) dir)))
    result))

;;; Variables
(defvar user-emacs-site-lisp-directory (expand-file-name (join-paths user-emacs-directory "site-lisp")))
(defvar user-emacs-lisp-directory (expand-file-name (join-paths user-emacs-directory "lisp")))
(defvar user-custom-file (expand-file-name (format "%s.el" user-login-name) user-emacs-directory))
(defvar user-home-directory (expand-file-name "~"))
(defvar user-lab-directory (expand-file-name "lab" user-home-directory))
(defvar user-stash-directory (expand-file-name ".stash" user-home-directory))
(defvar user-store-directory (expand-file-name ".store" user-home-directory))
(defvar user-mail-directory (expand-file-name "mail" user-home-directory))
(defvar user-org-stash-directory (expand-file-name "org" user-stash-directory))
(defvar default-theme 'modus-vivendi)
(defvar company-source-directory (join-paths user-home-directory "comp"))
(defvar company-org-directory (join-paths company-source-directory "org"))
(defvar company-babel-file (join-paths company-org-directory "meta/babel.org"))
(defvar company-bibliography (join-paths company-org-directory "graph/refs.bib"))
(defvar company-domain "compiler.company")
(defvar company-name "The Compiler Company, LLC")
(defvar company-vc-domain "vc.compiler.company")
(defvar company-vc-url (format "https://%s" company-vc-domain))
(defvar company-home "the.compiler.company")
(defvar company-cdn-url "https://cdn.compiler.company")
(defvar emacs-config-source (join-paths company-source-directory "core/emacs"))

;;; Settings
(setopt desktop-dirname (expand-file-name "sessions" user-emacs-directory)

;;; Load Path
(add-to-load-path 
 (expand-file-name "site-lisp" user-emacs-directory)
 user-emacs-site-lisp-directory (join-paths company-source-directory "core/slime"))

;;; Package Setup
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
	("melpa" . "https://melpa.org/packages/")
	))

;; use-pacakge defaults
(use-package use-package-ensure-system-package)
(use-package diminish)
(use-package delight)
(setopt 
 use-package-always-defer t
 use-package-expand-minimally t)

;;; Enable Commands
(put 'upcase-region 'disabled nil)
(put 'list-threads 'disabled nil)
(put 'list-timers 'disabled nil)

;;; Global Modes
(kill-ring-deindent-mode)

;;; Load default.el
(dolist (x '("util.el" "default.el" "keys.el" "config.el"))
  (let ((y (concat user-emacs-directory x)))
    (when (file-exists-p y)
      (load y nil t))))

(add-hook 'after-init-hook (load-keys))
(add-hook 'after-init-hook (if (and (boundp 'user-custom-file) (file-exists-p user-custom-file))
	                           (load-file user-custom-file)))
(add-hook 'after-init-hook 'load-default-theme)
