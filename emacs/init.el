;;; init.el --- emacs init -*- lexical-binding: t -*-

;; default init file for GNU Emacs.

;;; Code:
(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))

(dolist (x '("util.el" "default.el" "keys.el"))
  (let ((y (concat user-emacs-directory x))
        (byte-compile-warnings nil)
        (native-comp-async-warnings-errors-kind nil))
    (if (and (native-comp-available-p) (not (eq system-type 'darwin)))
         (native-compile y)
         (byte-compile-file y))
	 (load y nil t)))

(add-hook 'after-init-hook (load-keys))

(add-hook 'after-init-hook (if (and (boundp 'user-custom-file) (file-exists-p user-custom-file))
	                       (load-file user-custom-file)))

(add-hook 'after-init-hook 'load-default-theme)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c3e62e14eb625e02e5aeb03d315180d5bb6627785e48f23ba35eb7b974a940af"
     "01cad03be8c042a9941fda5a484280629ee2cc83fe084af6d19376c83141c91b"
     "79ab8329f4522beaa2285888d38f6204bb60f324912660d774a412a79e336d6c"
     "9ddb83c12595e789e9abd04a5c0705661748776223a794a6f64669352b956e79"
     "29a073e66535bad18e11e9bcaa17d7f2d17e4c79f01023e59e9841633915c232"
     "587ce9a1a961792114991fd488ef9c3fc37f165f6fea8b89d155640e81d165a3"
     "8a3d04fd24afde8333c1437a3ecaa616f121554041a4e7e48f21b28f13b50246"
     "4f03e70554a58349740973c69e73aefd8ce761a77b22a9dc52a19e708532084a"
     "0a953c81f5798aa99cafbc4aa8a56d16827442400028f6c1eab0c43061ea331c"
     "b93039071f490613499b76c237c2624ae67a9aafbc717da9b4d81f456344e56e"
     default)))
