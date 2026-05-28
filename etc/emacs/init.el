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

(defun mkstr (&rest args)
  "Paul Graham's mkstr utility from On Lisp.

Coerce ARGS into a single string and return it."
  (let* ((s ""))
    (dolist (a args)
      (cond
       ((null a) nil)
       ((sequencep a) (setq s (concat s a)))
       ((numberp a) (setq s(concat s (number-to-string a))))
       ((symbolp a) (setq s(concat s (symbol-name a))))))
    s))

(defun symb (&rest args)
  "Concat ARGS and return a newly interned symbol."
  (intern (apply #'mkstr args)))

(defun group (source n)
  "Group a list of arguments SOURCE by any provided grouping amount N.

For example:
(group (quote (foo 2 bar 4)) 2) ;=> ((foo 2) (bar 4))
(group (quote (a b c d e f)) 3) ;=> ((a b c) (d e f))
"
  (when (zerop n) (error "zero length"))
  (cl-labels ((rec (source acc)
		        (let ((rest (nthcdr n source)))
		          (if (consp rest)
			          (rec rest (cons
				                 (cl-subseq source 0 n)
				                 acc))
		            (nreverse
			         (cons source acc))))))
    (when source (rec source nil))))

(defun flatten (x)
  "Given a tree X, return all the leaves of the tree."
  (cl-labels ((rec (x acc)
		        (cond ((null x) acc)
			          ((atom x) (cons x acc))
			          (t (rec
			              (car x)
			              (rec (cdr x) acc))))))
    (rec x nil)))

(defun intersperse (element list)
  "Intersperse ELEMENT between each element of LIST."
  (if (null list)
      nil
    (cons (car list)
	      (cl-mapcan (lambda (x) (list element x)) (cdr list)))))

(defun hash-table-alist (table)
  "Returns an association list containing the keys and values of hash table
TABLE."
  (let ((alist nil))
    (maphash (lambda (k v)
	           (push (cons k v) alist))
	         table)
    (nreverse alist)))

(defun load-default-theme (&optional theme)
  (interactive)
  (when theme (setq default-theme theme))
  (load-theme default-theme t))

(defun load-keys (&optional custom)
  (let ((keydefs (or custom (join-paths user-emacs-site-lisp-directory "keymaps.el"))))
    (load keydefs nil t)))

(defun gen-site-lisp-autoloads ()
  (interactive)
  (loaddefs-generate 
   (list
    user-emacs-site-lisp-directory
    (join-paths user-emacs-site-lisp-directory "slime"))
   (join-paths user-emacs-site-lisp-directory "autoloads.el")))

(defun gen-lisp-autoloads ()
  (interactive)
  (loaddefs-generate user-emacs-lisp-directory
                     (join-paths user-emacs-lisp-directory "autoloads.el")))

(defun upgrade-emacs (&optional ask)
  (interactive)
  (package-refresh-contents)
  (package-install-selected-packages (not ask))
  (package-upgrade-all ask))

;;; Variables
(defvar user-emacs-site-lisp-directory (expand-file-name (join-paths user-emacs-directory "site-lisp")))
(defvar user-emacs-lisp-directory (expand-file-name (join-paths user-emacs-directory "lisp")))
(defvar user-custom-file (expand-file-name (format "%s.el" user-login-name) user-emacs-directory))
(defvar user-config-file (join-paths user-emacs-directory "config.el"))
(defvar user-home-directory (expand-file-name "~"))
(defvar user-lab-directory (expand-file-name "lab" user-home-directory))
(defvar user-stash-directory (expand-file-name ".stash" user-home-directory))
(defvar user-store-directory (expand-file-name ".store" user-home-directory))
(defvar user-mail-directory (expand-file-name "mail" user-home-directory))
(defvar user-org-stash-directory (expand-file-name "org" user-stash-directory))
(defvar default-theme 'modus-vivendi)
(defvar company-source-directory (join-paths user-home-directory "src"))
(defvar company-org-directory (join-paths company-source-directory "org"))
(defvar company-babel-file (join-paths company-org-directory "meta/babel.org"))
(defvar company-bibliography (join-paths company-org-directory "graph/refs.bib"))
(defvar company-domain "compiler.company")
(defvar company-name "The Compiler Company, LLC")
(defvar company-vc-domain "vc.compiler.company")
(defvar company-url (format "https://%s" company-domain))
(defvar company-vc-url (format "https://%s" company-vc-domain))
(defvar company-packy-domain "packy.compiler.company")
(defvar company-packy-url (format "https://%s" company-packy-domain))
(defvar company-home "the.compiler.company")
(defvar company-cdn-url "https://cdn.compiler.company")
(defvar emacs-config-source (join-paths company-source-directory "core/emacs"))
(defvar core-lisp-program "/usr/bin/core")

;;; Settings
(setopt custom-file (expand-file-name "custom.el" user-emacs-directory)
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

;;; Load Path
(add-to-load-path 
 (expand-file-name "site-lisp" user-emacs-directory)
 user-emacs-lisp-directory
 user-emacs-site-lisp-directory 
 (join-paths user-emacs-site-lisp-directory "slime"))

;; Load autoloads if they exist
(require 'autoloads (join-paths user-emacs-site-lisp-directory "autoloads") t)

;;; Package Setup
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	    ("nongnu" . "https://elpa.nongnu.org/nongnu/")
	    ("melpa" . "https://melpa.org/packages/")))

;; use-package defaults
(use-package system-packages :ensure t)
(use-package diminish :ensure t)
(use-package delight :ensure t)
(setq use-package-expand-minimally t)

;;; Enable Commands
(put 'upcase-region 'disabled nil)
(put 'list-threads 'disabled nil)
(put 'list-timers 'disabled nil)

;;; Global Modes
(kill-ring-deindent-mode t)
(repeat-mode t)
(desktop-save-mode t)

;;; Load default.el
(load (join-paths user-emacs-directory "default.el"))

;;; Load user customizations
(when (file-exists-p user-config-file) (load-file user-config-file))
(when (file-exists-p user-custom-file) (load-file user-custom-file))
