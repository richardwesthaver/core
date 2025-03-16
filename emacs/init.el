;;; init.el --- emacs init -*- lexical-binding: t -*-

;; default init file for GNU Emacs.

;;; Code:
(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))

;;; Packages
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
	;; melpa is 429in us.. :C
	;; ("melpa" . "https://melpa.org/packages/")
	))
(setopt
 use-package-always-ensure t
 use-package-expand-minimally t)

(dolist (x '("util.el" "default.el" "keys.el"))
  (let ((y (concat user-emacs-directory x)))
    (load y nil t)))

(add-hook 'after-init-hook (load-keys))

(add-hook 'after-init-hook (if (and (boundp 'user-custom-file) (file-exists-p user-custom-file))
	                           (load-file user-custom-file)))

(add-hook 'after-init-hook 'load-default-theme)
