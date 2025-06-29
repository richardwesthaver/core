;;; bar.el --- Speedbar extensions -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Richard Westhaver

;; Author: Richard Westhaver <richard.westhaver@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is currently just a cheap copy of sr-speedbar:
;; http://www.emacswiki.org/emacs/download/sr-speedbar.el

;;; Code:
(require 'speedbar)
(require 'advice)
(require 'cl-lib)

(defgroup bar nil
  "Same frame speedbar."
  :group 'speedbar)

(defcustom bar-default-width 20
  "Initial width of `bar-window' under window system."
  :type 'integer
  :group 'bar)

(defcustom bar-max-width 58
  "The max width limit that window allowed.
Default, if hide `bar' window will remember
window width, except the window width larger than
this value."
  :type 'integer
  :group 'bar)

(defcustom bar-auto-refresh t
  "Automatically refresh speedbar content when changed directory.
Default is t."
  :type 'boolean
  :set (lambda (symbol value)
	 (set symbol value))
  :group 'bar)

(defcustom bar-right-side nil
  "Show the speedbar to the right side of the current window.
If non-nil, the speedbar will appear on the right."
  :type 'boolean
  :set (lambda (symbol value)
	 (set symbol value))
  :group 'bar)

(defcustom bar-delete-windows nil
  "Allow the speedbar to delete other windows before showing up.
If nil, speedbar will not touch your window configuration.
Otherwise `delete-other-windows' will be called before showing
the speedbar.

Default is nil."
  :type 'boolean
  :group 'bar)

(defcustom bar-use-frame-root-window nil
  "Open speedbar based on selected window or frame root window.
If nil, the speedbar window will split from `selected-window'.
Otherwise `frame-root-window'.
Default is nil."
  :type 'boolean
  :set (lambda (symbol value)
	 (set symbol value))
  :group 'bar)

(if (not (fboundp 'ad-advised-definition-p))
    (defun ad-advised-definition-p (definition)
      "Return non-nil if DEFINITION was generated from advice information."
      (if (or (ad-lambda-p definition)
	      (macrop definition)
	      (ad-compiled-p definition))
	  (let ((docstring (ad-docstring definition)))
	    (and (stringp docstring)
		 (get-text-property 0 'dynamic-docstring-function docstring))))))

(defun bar-handle-other-window-advice (activate)
  "Handle advice for function `other-window'.
If ACTIVATE is `non-nil' enable advice `bar-other-window-advice'.
Otherwise disable it."
  (if activate
      (ad-enable-advice 'other-window 'after 'bar-other-window-advice)
    (ad-disable-advice 'other-window 'after 'bar-other-window-advice))
  (ad-activate 'other-window))

(defcustom bar-skip-other-window-p nil
  "Whether skip `bar' window with `other-window'.
Default, can use `other-window' select window in cyclic
ordering of windows.  But sometimes we don't want select
`bar' window use `other-window'.
Just want make `bar' window as a view sidebar.

So please turn on this option if you want skip
`bar' window with `other-window'.

Default is nil."
  :type 'boolean
  :set (lambda (symbol value)
	 (set symbol value)
	 (if (fboundp 'ad-advised-definition-p)
	     (when (ad-advised-definition-p 'other-window)
	       (bar-handle-other-window-advice value))
	   (when (ad-is-advised 'other-window)
	     (bar-handle-other-window-advice value))))
  :group 'bar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Constant ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst bar-buffer-name "*SPEEDBAR*"
  "The buffer name of bar.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar bar-width bar-default-width
  "Initial width of speedbar-window.")

(defvar bar-window nil
  "Speedbar window.")

(defvar bar-last-refresh-dictionary nil
  "The last refresh dictionary record of 'bar-refresh'.")

(eval-when-compile
  (defvar ecb-activated-window-configuration nil)
  (defun ecb-activate ())
  (defun ecb-deactivate ()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun bar-toggle ()
  "Toggle bar window.
Toggle visibility of bar by resizing
the `bar-window' to a minimal width
or the last width when visible.
Use this function to create or toggle visibility
of a speedbar-window.  It will be created if necessary."
  (interactive)
  (if (bar-exist-p)
      (bar-close)
    (bar-open)))

;;;###autoload
(defun bar-open ()
  "Create `bar' window."
  (interactive)
  (if (not (bar-exist-p))
      (let ((current-window (selected-window)))
	;; Ensure only one window is there
	;; when `bar-delete-windows' is non-nil
	(if bar-delete-windows
	    (delete-other-windows))
	;; Whether activate `other-window' advice
	;; to skip `bar' window when use `other-window'.
	(bar-handle-other-window-advice bar-skip-other-window-p)
	;; Switch buffer
	(if (bar-buffer-exist-p speedbar-buffer)
	    (unless (bar-window-exist-p bar-window)
	      (bar-get-window))
	  (if (<= (bar-current-window-take-width) bar-width)
	      (setq bar-width bar-default-width))
	  (bar-get-window)             ;get `bar' window that split current window
	  (setq speedbar-buffer (get-buffer-create bar-buffer-name)
		speedbar-frame (selected-frame)
		dframe-attached-frame (selected-frame)
		speedbar-select-frame-method 'attached
		speedbar-verbosity-level 0 ;don't say anything, i don't like ... :)
		speedbar-last-selected-file nil)
	  (set-buffer speedbar-buffer)
	  (buffer-disable-undo speedbar-buffer) ;make disable in speedbar buffer, otherwise will occur `undo-outer-limit' error
	  (speedbar-mode)
	  (speedbar-reconfigure-keymaps)
	  (speedbar-update-contents)
	  (speedbar-set-timer 1)
	  ;; Add speedbar hook.
	  (add-hook 'speedbar-before-visiting-file-hook 'bar-before-visiting-file-hook t)
	  (add-hook 'speedbar-before-visiting-tag-hook 'bar-before-visiting-tag-hook t)
	  (add-hook 'speedbar-visiting-file-hook 'bar-visiting-file-hook t)
	  (add-hook 'speedbar-visiting-tag-hook 'bar-visiting-tag-hook t)
	  ;; Add `kill-buffer-hook'.
	  (add-hook 'kill-buffer-hook 'bar-kill-buffer-hook) ;add `kill-buffer-hook'
	  ;; Auto refresh speedbar content
	  ;; if option `bar-auto-refresh' is non-nil
	  (bar-handle-auto-refresh bar-auto-refresh))
	(set-window-buffer bar-window (get-buffer bar-buffer-name))
	(set-window-dedicated-p bar-window t) ;make `bar-window' dedicated to speedbar-buffer.
	(select-window current-window))
    (message "`bar' window has exist.")))

(defun bar-close ()
  "Close `bar' window and save window width."
  (interactive)
  (if (bar-exist-p)
      (let ((current-window (selected-window)))
	;; Remember window width.
	(bar-select-window)
	(bar-remember-window-width)
	;; Close window.
	(if (and (require 'ecb nil t)
		 ecb-activated-window-configuration)
	    ;; Toggle ECB window when ECB window activated.
	    (progn
	      (ecb-deactivate)
	      (ecb-activate))
	  ;; Otherwise delete dedicated window.
	  (delete-window bar-window)
	  (if (bar-window-exist-p current-window)
	      (select-window current-window))))
    (message "`bar' window is not exist.")))

(defun bar-select-window ()
  "Force the windows that contain `bar'."
  (interactive)
  (if (bar-exist-p)
      (select-window bar-window)
    (message "`bar' window is not exist.")))

(defun bar-refresh-turn-on ()
  "Turn on refresh content automatically."
  (interactive)
  (setq bar-auto-refresh t)
  (bar-handle-auto-refresh bar-auto-refresh t))

(defun bar-refresh-turn-off ()
  "Turn off refresh content automatically."
  (interactive)
  (setq bar-auto-refresh nil)
  (bar-handle-auto-refresh bar-auto-refresh t))

(defun bar-refresh-toggle ()
  "Toggle refresh content status."
  (interactive)
  (setq bar-auto-refresh (not bar-auto-refresh))
  (bar-handle-auto-refresh bar-auto-refresh t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; utilise functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bar-exist-p ()
  "Return `non-nil' if `bar' is exist.
Otherwise return nil."
  (and (bar-buffer-exist-p speedbar-buffer)
       (bar-window-exist-p bar-window)))

(defun bar-window-p ()
  "Return `non-nil' if current window is `bar' window.
Otherwise return nil."
  (equal bar-buffer-name (buffer-name (window-buffer))))

(defun bar-remember-window-width ()
  "Remember window width."
  (let ((win-width (bar-current-window-take-width)))
    (if (and (bar-window-p)
	     (> win-width 1)
	     (<= win-width bar-max-width))
	(setq bar-width win-width))))

(defun bar-get-window ()
  "Get `bar' window."
  (setq bar-window
	(split-window (if bar-use-frame-root-window
			  (frame-root-window)
			(selected-window))
		      (- bar-width)
		      (if bar-right-side 'right 'left))))

(defun bar-before-visiting-file-hook ()
  "Function that hook `speedbar-before-visiting-file-hook'."
  (select-window (get-mru-window)))

(defun bar-before-visiting-tag-hook ()
  "Function that hook `speedbar-before-visiting-tag-hook'."
  (select-window (get-mru-window)))

(defun bar-visiting-file-hook ()
  "Function that hook `speedbar-visiting-file-hook'."
  (select-window (get-mru-window)))

(defun bar-visiting-tag-hook ()
  "Function that hook `speedbar-visiting-tag-hook'."
  (select-window (get-mru-window)))

(defun bar-kill-buffer-hook ()
  "Function that hook `kill-buffer-hook'."
  (when (eq (current-buffer) speedbar-buffer)
    (setq speedbar-frame nil
	  dframe-attached-frame nil
	  speedbar-buffer nil)
    (speedbar-set-timer nil)
    (remove-hook 'speedbar-before-visiting-file-hook 'bar-before-visiting-file-hook)
    (remove-hook 'speedbar-before-visiting-tag-hook 'bar-before-visiting-tag-hook)
    (remove-hook 'speedbar-visiting-file-hook 'bar-visiting-file-hook)
    (remove-hook 'speedbar-visiting-tag-hook 'bar-visiting-tag-hook)))

(defun bar-refresh ()
  "Refresh the context of speedbar."
  (when (and (not (equal default-directory bar-last-refresh-dictionary)) ;if directory is change
	     (not (bar-window-p))) ;and is not in speedbar buffer
    (setq bar-last-refresh-dictionary default-directory)
    (speedbar-refresh)))

(defun bar-handle-auto-refresh (activate &optional echo-show)
  "Automatically refresh speedbar content when changed directory.
Do nothing if option ACTIVATE is nil.
Will display message if ECHO-SHOW is non-nil."
  (if activate
      (progn
	(add-hook 'speedbar-timer-hook 'bar-refresh)
	(if echo-show (message "Turn on speedbar content refresh automatically.")))
    (remove-hook 'speedbar-timer-hook 'bar-refresh)
    (if echo-show (message "Turn off speedbar content refresh automatically."))))

(defun bar-current-window-take-width (&optional window)
  "Return the width that WINDOW take up.
If WINDOW is nil, get current window."
  (let ((edges (window-edges window)))
    (- (nth 2 edges) (nth 0 edges))))

(defun bar-window-dedicated-only-one-p ()
  "Only have one non-dedicated window."
  (interactive)
  (let ((window-number 0)
	(dedicated-window-number 0))
    (walk-windows
     (lambda (w)
       (with-selected-window w
	 (cl-incf window-number)
	 (if (window-dedicated-p w)
	     (cl-incf dedicated-window-number)))))
    (if (and (> dedicated-window-number 0)
	     (= (- window-number dedicated-window-number) 1))
	t nil)))

(defun bar-window-exist-p (window)
  "Return `non-nil' if WINDOW is exist.
Otherwise return nil."
  (and window (window-live-p window)))

(defun bar-buffer-exist-p (buffer)
  "Return `non-nil' if BUFFER is exist.
Otherwise return nil."
  (and buffer (buffer-live-p buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Advices ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice delete-other-windows (around bar-delete-other-window-advice activate)
  "This advice to make `bar' window can't deleted by command `delete-other-windows'."
  (let ((bar-active-p (bar-window-exist-p bar-window)))
    (if bar-active-p
	(let ((current-window (selected-window)))
	  (dolist (win (window-list))
	    (when (and (window-live-p win)
		       (not (eq current-window win))
		       (not (window-dedicated-p win)))
	      (delete-window win))))
      ad-do-it)))

(defadvice delete-window (before bar-delete-window-advice activate)
  "This advice to remember `bar' window width before deleted.
Use `delete-window' delete `bar' window have same effect as `bar-close'."
  ;; Remember window width before deleted.
  (bar-remember-window-width))

(defadvice pop-to-buffer (before bar-pop-to-buffer-advice activate)
  "This advice is to fix `pop-to-buffer' problem with dedicated window.
Default, function `display-buffer' can't display buffer in select window
if current window is `dedicated'.

So function `display-buffer' conflict with `bar' window, because
`bar' window is `dedicated' window.

That is to say, when current frame just have one `non-dedicated' window,
any functions that use `display-buffer' can't split windows
to display buffer, even option `pop-up-windows' is enable.

And the example function that can occur above problem is `pop-to-buffer'."
  (when (and pop-up-windows                            ;`pop-up-windows' is enable
	     (bar-window-dedicated-only-one-p) ;just have one `non-dedicated' window
	     (bar-window-exist-p bar-window)
	     (not (bar-window-p)) ;not in `bar' window
	     (not (bound-and-true-p helm-alive-p)))
    (split-window-vertically)
    (windmove-down)))

(defadvice other-window (after bar-other-window-advice)
  "Default, can use `other-window' select window in cyclic ordering of windows.
But sometimes we don't want select `bar' window use `other-window'.
Just want make `bar' window as a view sidebar.

This advice can make `other-window' skip `bar' window."
  (let ((count (or (ad-get-arg 0) 1)))
    (when (and (bar-window-exist-p bar-window)
	       (eq bar-window (selected-window)))
      (other-window count))))

;; (speedbar-add-supported-extension "skelfile")
(speedbar-add-supported-extension ".lisp")
(speedbar-add-supported-extension ".rs")
(speedbar-add-supported-extension ".el.gz")

(provide 'bar)
;;; bar.el ends here
