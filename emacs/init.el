;;; init.el --- emacs init -*- lexical-binding: t -*-

;; default init file for GNU Emacs.

;;; Code:
(load-file "util.el")
(add-to-load-path (join-paths user-emacs-directory "lib/"))
(load-file "default.el")
(package-install-selected-packages)
