;;; init.el --- emacs init -*- lexical-binding: t -*-

;; default init file for GNU Emacs.

;;; Code:
(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))

(dolist (x '("util.el" "default.el" "keys.el"))
  (let ((y (concat user-emacs-directory x)))
    (setf byte-compile-warnings nil
          native-comp-async-warnings-errors-kind nil)
    (if (and (native-comp-available-p) (not (eq system-type 'darwin)))
         (native-compile y)
         (byte-compile-file y))
	 (load y nil t)))

(add-hook 'after-init-hook (load-keys))

(add-hook 'after-init-hook (if (and (boundp 'user-custom-file) (file-exists-p user-custom-file))
	                       (load-file user-custom-file)))

(add-hook 'after-init-hook 'load-default-theme)
