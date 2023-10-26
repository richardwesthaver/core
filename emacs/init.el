;;; init.el --- emacs init -*- lexical-binding: t -*-

;; default init file for GNU Emacs.

;;; Code:
(require 'init/util)

(add-to-load-path (join-paths user-emacs-directory "lib/"))

(require 'default)
