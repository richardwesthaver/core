;;; keys.el --- emacs keys -*- lexical-binding: t -*-

;; default keybinds

;;; Commentary:

;; I encourage you to remap these keys as you see fit. Where possible,
;; wrap your custom bindings in a keymap instead of redefining the
;; global defaults defined here.

;;; Code:

(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c C-l") #'org-insert-link-global)
(global-set-key (kbd "C-c C-o") #'org-open-at-point-global)

(provide 'keys)
;; keys.el ends here
