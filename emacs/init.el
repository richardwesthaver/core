;;; init.el --- emacs init -*- lexical-binding: t -*-

;; default init file for GNU Emacs.

;;; Code:
(dolist (x '("util.el" "default.el" "keys.el"))
  (load-file (concat user-emacs-directory x)))
(add-to-load-path (join-paths user-emacs-directory "lib/"))
