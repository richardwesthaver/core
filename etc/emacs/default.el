;;; default.el --- default config -*- lexical-binding:t -*-

;; Core Emacs Defaults

;;; Code:
;;; Settings
(require 'util)  
(setq
 org-safe-remote-resources '("\\`https://cdn\\.compiler\\.company/org/clean\\.theme\\'")
 ;; tabs = bad (unless in makefile..)
 tab-width 4
 switch-to-buffer-obey-display-actions t
show-paren-context-when-offscreen 'overlay
 indent-tabs-mode nil
 make-backup-files nil
 save-list-file-prefix (expand-file-name "auto-save/." user-emacs-directory)
 tramp-auto-save-directory (expand-file-name "auto-save/tramp/" user-emacs-directory)
 dired-free-space nil
 mml-attach-file-at-the-end t
 dired-mouse-drag-files t
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
 org-publish-timestamp-directory (join-paths user-emacs-directory ".org-timestamps/"))

;;; UI
(use-package hide-mode-line
  :hook (speedbar-mode . hide-mode-line-mode))

;;;; Icons
;; all-the-icons all-the-icons-dired all-the-icons-ibuffer ;; icons
(use-package icons
  :ensure t
  :config
  (use-package nerd-icons)
  (use-package nerd-icons-ibuffer :hook (ibuffer-mode . nerd-icons-ibuffer-mode))
  (use-package nerd-icons-dired :hook (dired-mode . nerd-icons-dired-mode))
  (use-package nerd-icons-corfu :hook (corfu-mode . nerd-icons-corfu-mode))
  (use-package nerd-icons-completion :hook (completion-mode . nerd-icons-completion-mode))
  (use-package nerd-icons-grep :hook (grep-mode . nerd-icons-grep-mode))
  (use-package nerd-icons-grep :hook (grep-mode . nerd-icons-grep-mode))
  (use-package nerd-icons-xref :hook (xref-mode . nerd-icons-xref-mode))
  (use-package tab-line-nerd-icons :hook (tab-line-mode . tab-line-nerd-icons-global-mode)))

;;; Whitespace
(use-package whitespace
  :init
  (setq
   whitespace-style '(face tabs trailing lines-tail indentation::space)
   whitespace-line-column 88))

;;; Env
(use-package exec-path-from-shell
  :init
  (add-to-list 'exec-path "/usr/bin/")
  (add-to-list 'exec-path "/usr/sbin/")
  (add-to-list 'exec-path "/usr/local/bin/")
  (add-to-list 'exec-path "/usr/local/share/lisp/")
  (add-to-list 'exec-path "/usr/share/lisp/")
  (add-to-list 'exec-path (join-paths user-home-directory ".local/bin/"))
  :config
  (exec-path-from-shell-copy-envs (list "SSH_AGENT_PID"
					"SSH_AUTH_SOCK"
					"PATH"
					"CARGO_HOME"
					"CC"
					"LD"
					"LD_LIBRARY_PATH"
					"RUSTUP_HOME"
					"QUICKLISP_HOME"
					"DEV" "DEV_ID" "DEV_HOME"
					"WORKER" "WORKER_ID" "WORKER_HOME"
					"SBCL_HOME"
					"STASH"
					"STORE"
					"LISP_HOME")))

;;; Completion
(use-package completion
  :init
  (setq
   completion-ignore-case t
   tab-always-indent 'complete))
(use-package corfu)
(use-package cape)
(use-package consult)
(use-package marginalia
  :config (marginalia-mode))
(use-package vertico
  :ensure t
  :config (vertico-mode)
  :bind 
  (("M-q" . #'vertico-quick-insert)
   ("C-q" . #'vertico-quick-exit)))
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-overrides '((file (styles basic partial-completion))
                                        (eglot (styles orderless))
                                        (eglot-capf (styles orderless)))))
(use-package company)
(use-package expand
  :hook ((expand-expand . indent-according-to-mode)
	 (expand-jump . indent-according-to-mode)))
(use-package completion-preview
  :config (global-completion-preview-mode))
(use-package corfu
  :after (completion)
  :config
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (setq corfu-popupinfo-delay (cons 0.8 0.2))
  (corfu-echo-mode)
  (dolist (c (list (cons "SPC" " ")
                   (cons "." ".")
                   (cons "," ",")
                   (cons ":" ":")
                   (cons ")" ")")
                   (cons "}" "}")
                   (cons "]" "]")))
    (define-key corfu-map (kbd (car c)) `(lambda ()
                                           (interactive)
                                           (corfu-insert)
                                           (insert ,(cdr c)))))
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
  (add-to-list 'completion-at-point-functions #'cape-abbrev t) 
  (add-to-list 'completion-at-point-functions #'cape-file)
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred)))))
  (keymap-set corfu-map "M-m" #'corfu-move-to-minibuffer)
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer))

;;; Dired
(use-package dired
  :config
  (setq dired-dwim-target t
	dired-free-space 'separate))

;;; Speedbar
(use-package speedbar
  :config
  (setq speedbar-sort-tags t
	speedbar-prefer-window t
	speedbar-track-mouse-flag t))

;;; Projects
(use-package package
  :init
  (setq project-list-file (expand-file-name "projects" user-emacs-directory)
          project-mode-line t
          project-file-history-behavior 'relativize)
  :config
  (defun remember-project ()
    (interactive)
    (project-remember-project (project-current))
    project--list))

;;; Tabs
(use-package tab-bar
  :hook #'tab-bar-history)

;;; Lisp
(use-package slime
  :hook 
  (lisp . (slime-mode slime-cape-enable))
  (inferior-lisp . inferior-slime-mode)
  (slime-repl . slime-cape-enable)
  :init
  (setq inferior-lisp-program (format "%s --dynamic-space-size=8G --control-stack-size=32"
				    (default-lisp))
      scheme-program-name "gsi"
      slime-auto-start t
      guile-program "guile"
      cmulisp-program "lisp"
      scsh-program "scsh"
      ;; rebind the defpackage-regexp function to include DEFPKG
      slime-defpackage-regexp
      "^(\\(cl:\\|common-lisp:\\|uiop:\\|uiop/package:\\|std:\\|std/defpkg:\\|pkg:\\)?\\(defpackage\\|define-package\\|defpkg\\)\\>[ \t']*"
      common-lisp-style-default "core"
      slime-threads-update-interval 1)

  :config
  (defun default-lisp ()
    (if (file-exists-p core-lisp-program)
	core-lisp-program
      "sbcl"))

  (defvar slime-toggle nil)
  (defun slime-switch-to-output (&optional same-window)
    "Select the output buffer, when possible in an existing window. When
SAME-WINDOW is non-nil open in the current window.

Hint: You can use `display-buffer-reuse-frames' and
`special-display-buffer-names' to customize the frame in which the
buffer should appear."
    (interactive)
    (let ((buffer (slime-output-buffer)))
      (if same-window
          (pop-to-buffer-same-window buffer)
	(pop-to-buffer buffer))))

  (defun slime-toggle ()
    "Toggle between current buffer and slime-repl."
    (interactive)
    (if (eq major-mode 'slime-repl-mode)
	(setq slime-toggle
              (pop-to-buffer-same-window
               (or slime-toggle (read-buffer "lisp buffer: "))))
      (if (slime-connected-p)
          (progn
            (setq slime-toggle (current-buffer))
            (slime-switch-to-output t))
	(setq slime-toggle (current-buffer))
	(slime))))

  (defun slime-load-script (filename)
    "Like `slime-load-file' but for script files containing a shebang
line (which is skipped)."
    (interactive (list
                  (read-file-name "Load file: " nil nil
                                  nil (if (buffer-file-name)
                                          (file-name-nondirectory
                                           (buffer-file-name))))))
    (let ((lisp-filename (slime-to-lisp-filename (expand-file-name filename))))
      ;; TODO 2026-05-01: 
      (slime-eval-with-transcript `(swank:load-script-file ,lisp-filename))))

  (defvar lisp-toggle nil)

  (defun lisp-toggle (&optional cmd)
    "Toggle between current buffer and inferior-lisp process buffer."
    (interactive)
    (if (eq major-mode 'inferior-lisp-mode)
	(pop-to-buffer-same-window
	 (or lisp-toggle (read-buffer "lisp buffer: ")))
      (if inferior-lisp-buffer
          (progn
            (setq lisp-toggle (current-buffer))
            (inferior-lisp (or cmd inferior-lisp-program)))
	(setq lisp-toggle (current-buffer))
	(inferior-lisp (or cmd inferior-lisp-program)))))

  (setq slime-contribs '(slime-fancy
			 slime-quicklisp
			 slime-hyperdoc
			 ;; slime-listener-hooks
			 ;; slime-enclosing-context
			 ;; slime-media
			 slime-mrepl
			 ;; slime-company
			 slime-sbcl-exts
			 slime-cape ;; ext
			 slime-repl-ansi-color
			 ;; slime-cl-indent
			 ;; slime-snapshot
			 slime-sprof
			 slime-tramp
			 ;; slime-typeout-frame
			 slime-xref-browser
			 ;; slime-highlight-edits
			 slime-repl-ansi-color))
  (slime-setup slime-contribs)
  (define-common-lisp-style
   "core"
   "Core Common Lisp Indentation Style"
   (:inherit "sbcl")
   (:indentation
    (defpkg (as defpackage))
    (make-instance 1)
    (reinitialize-instance 1)
    (ensure-package 1)
    (init 1)
    (defpackage* (as defpackage))
    (blasfunc 2)
    (symbol-call 2)
    (org-parse 2)
    (lety (as let))
    (lety* (as let*))
    (letv (as let))
    (letv* (as let*))
    (deferror (as define-condition))
    (defcondition (as define-condition))
    (plet (as let))
    (acase (as case))
    (atypecase (as typecase))
    (defwarning (as define-condition))
    (make-db (as make-instance))
    (make-schema (as make-instance))
    (make-simple-schema (as make-instance))
    (make-palette (as defpackage))
    (define-package (as defpackage))
    (defkernel (as defclass))
    (defhook (as defmacro))
    (defcommand (as defun))
    (define-cli (as make-instance))
    (walk-directory 1)
    (using-gensyms (as with-gensyms))
    (binding-gensyms (as with-gensyms))
    (if-let* (as if-let))
    (when-let* (as when-let))
    (load-config 1)
    (with-db 1)
    (incf 1)
    (decf 1)
    (make-load-form-saving-slots 1)
    (defconfig (as defclass))
    (defclass* (as defclass))
    (defsclass (as defclass))))

  (defun slime-connect-file (file &optional host)
    "Connect to the port number stored in FILE which should be the same value
as the first argument to SWANK:START-SERVER on the Lisp side."
    (interactive "fswank file: ")
    (slime-connect
     (or host "localhost")
     (string-to-number
      (with-temp-buffer
	(insert-file-contents file)
	(buffer-string))))))

;;; Asm
(use-package nasm-mode
  :hook (asm . nasm-mode))

;;; Rust
(use-package rust-mode 
  :ensure nil
  :hook (rust . eglot-ensure)
  :init
  (setq rust-rustfmt-switches nil
	rust-indent-offset 2))

;;; Python
(use-package python
  :hook eglot-ensure
  :init (setq python-indent-offset 2))

;;; Javascript
(use-package js
  :init (setq js-indent-level 2))
(use-package css-mode
  :init (setq css-indent-offset 2))

;;; Shell
(use-package shell
  :init (setq sh-basic-offset 2))

;;; Keyboard Macros
(use-package kmacro
  :config
  (defun toggle-macro-recording ()
    (interactive)
    (if defining-kbd-macro
	(end-kbd-macro)
      (start-kbd-macro nil)))

  (defun play-macro-if-not-playing ()
    (interactive)
    (if defining-kbd-macro
	(end-kbd-macro)
      (call-last-kbd-macro))))

;;; Registers
(use-package register
  :config
  (defun decrement-register (number register)
    "Subtract NUMBER from the contents of register REGISTER.
Interactively, NUMBER is the prefix arg."
    (interactive "p\ncDecrement register: ")
    (increment-register (- number) register))

  (defun copy-register (a b)
    "Copy register A to B."
    (interactive
     (list (register-read-with-preview "From register: ")
           (register-read-with-preview "To register: ")))
    (set-register b (get-register a)))

  (defun buffer-to-register (register &optional delete)
    "Put current buffer in register - this would also work for
  just buffers, as switch-to-buffer can use both, but it
  facilitates for easier saving/restoring of registers."
    (interactive "cPut current buffername in register: \nP.")
    (set-register register (cons 'buffer (buffer-name (current-buffer)))))

  (defun file-to-register (register &optional delete)
    "This is better than put-buffer-in-register for file-buffers, because a closed
   file can be opened again, but does not work for no-file-buffers."
    (interactive "cPut the filename of current buffer in register: \nP")
    (set-register register (cons 'file (buffer-file-name (current-buffer)))))

  (defun file-query-to-register (register &optional delete)
    (interactive
     (list
      (register-read-with-preview "File query to register: ")))
    (set-register register (list 'file-query (buffer-file-name (current-buffer)) (point)))))

;;; Outlines
(use-package outline
  :init (setq outline-minor-mode-use-buttons nil)
  :config
  (defun outline-hook (&optional rx)
    "Enable `outline-minor-mode' and set `outline-regexp'."
    (when rx (setq-local outline-regexp rx))
    (outline-minor-mode 1))

  (defun add-outline-hook (mode &optional rx)
    (let ((sym (symb mode "-hook")))
      (add-hook sym (lambda () (outline-hook rx)))))

  (defmacro outline-hooks (&rest pairs)
    `(mapc (lambda (x) (add-outline-hook (car x) (cadr x))) ',pairs))

  (outline-hooks (asm-mode ";;;+")
		 (nasm-mode ";;;+")
		 (rust-mode "\\(//!\\|////+\\)")
		 (sh-mode "###+")
		 (sh-script-mode "###+")
		 (makefile-mode "###+")
		 (conf-mode "###+")
		 (common-lisp-mode)
		 (emacs-lisp-mode)
		 (lisp-data-mode)
		 (org-mode)
		 (css-mode)
		 (html-mode)
		 (skel-mode)))

;;; Shell
(defun set-no-process-query-on-exit ()
  (let ((proc (get-buffer-process (current-buffer))))
    (when (processp proc)
      (set-process-query-on-exit-flag proc nil))))

(add-hook 'shell-mode-hook 'set-no-process-query-on-exit)
(add-hook 'term-exec-hook 'set-no-process-query-on-exit)

;;; Eshell
(use-package eshell
  :hook (eshell . (lambda ()
		    (eshell/alias "d" "dired $1")
		    (eshell/alias "ff" "find-file $1")
		    (eshell/alias "hgfe" "hg-fast-export.sh")))
  :init
  (setq eshell-highlight-prompt t
	eshell-hist-ignoredups t
	eshell-save-history-on-exit t
	eshell-prefer-lisp-functions nil
	eshell-destroy-buffer-when-process-dies t)
  :config
  (defun eshell-new()
    "Open a new instance of eshell."
    (interactive)
    (eshell 'Z))

  (defun eshell/clear ()
    "Clear the eshell buffer."
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))

  (defun eshell-quit-or-delete-char (arg)
    (interactive "p")
    (if (and (eolp) (looking-back eshell-prompt-regexp))
	(progn
          (eshell-life-is-too-much) ; Why not? (eshell/exit)
          (ignore-errors
            (delete-window)))
      (delete-forward-char arg)))

  (add-hook 'eshell-mode-hook
            (lambda ()
            (bind-keys :map eshell-mode-map
                       ("C-d" . eshell-quit-or-delete-char))))

  (defun eshell-next-prompt (n)
    "Move to end of Nth next prompt in the buffer. See `eshell-prompt-regexp'."
    (interactive "p")
    (re-search-forward eshell-prompt-regexp nil t n)
    (when eshell-highlight-prompt
      (while (not (get-text-property (line-beginning-position) 'read-only) )
	(re-search-forward eshell-prompt-regexp nil t n)))
    (eshell-skip-prompt))

  (defun eshell-previous-prompt (n)
    "Move to end of Nth previous prompt in the buffer. See `eshell-prompt-regexp'."
    (interactive "p")
    (backward-char)
    (eshell-next-prompt (- n)))

  (defun eshell-insert-history ()
    "Displays the eshell history to select and insert back into your eshell."
    (interactive)
    (insert (ido-completing-read "Eshell history: "
				 (delete-dups
                                  (ring-elements eshell-history-ring))))))

;;; Eww
(use-package shr
  :init
  (setq shr-use-colors nil
	shr-use-fonts nil
	shr-max-image-proportion 0.6
	shr-image-animate nil
	shr-discard-aria-hidden t
	shr-use-xwidgets-for-media t)
  :custom
  ;; ref: https://github.com/oantolin/emacs-config/blob/master/my-lisp/shr-heading.el
  (defun shr-heading-next (&optional arg)
    "Move forward by ARG headings (any h1-h4).
If ARG is negative move backwards, ARG defaults to 1."
    (interactive "p")
    (unless arg (setq arg 1))
    (catch 'return
      (dotimes (_ (abs arg))
	(when (> arg 0) (end-of-line))
	(if-let* ((match
		   (funcall (if (> arg 0)
				#'text-property-search-forward
			      #'text-property-search-backward)
			    'face '(shr-h1 shr-h2 shr-h3 shr-h4)
			    (lambda (tags face)
			      (cl-loop for x in (if (consp face) face (list face))
				       thereis (memq x tags))))))
	    (goto-char
	     (if (> arg 0) (prop-match-beginning match) (prop-match-end match)))
	  (throw 'return nil))
	(when (< arg 0) (beginning-of-line)))
      (beginning-of-line)
      (point)))

  (defun shr-heading-previous (&optional arg)
    "Move backward by ARG headings (any h1-h4).
If ARG is negative move forwards instead, ARG defaults to 1."
    (interactive "p")
    (shr-heading-next (- (or arg 1))))

  (defun shr-heading--line-at-point ()
    "Return the current line."
    (buffer-substring (line-beginning-position) (line-end-position)))

  (defun shr-heading-setup-imenu ()
    "Setup imenu for h1-h4 headings in eww buffer.
Add this function to appropriate major mode hooks such as
`eww-mode-hook' or `elfeed-show-mode-hook'."
    (setq-local
     imenu-prev-index-position-function #'shr-heading-previous
     imenu-extract-index-name-function  #'shr-heading--line-at-point))

  (defvar shr-heading-map
    (let ((map (make-sparse-keymap)))
      (define-key map "n" #'shr-heading-next)
      (define-key map "\C-n" #'shr-heading-next)
      (define-key map "p" #'shr-heading-previous)
      (define-key map "\C-p" #'shr-heading-previous)
      map)))

(use-package eww
  :after (shr)
  :hook ((eww-mode .shr-heading-setup-imenu)
	 (eww-mode . (lambda () (define-key eww-mode-map "i" shr-heading-map))))
  :config
  (setopt
   browse-url-browser-function 'eww
   eww-auto-rename-buffer 'title
   eww-search-prefix "https://html.duckduckgo.com/html/?q=")
  (defun eww-at-point ()
    (interactive)
    (eww (thing-at-point 'url))))

;;; Tramp
(use-package tramp
  :init
  (setq tramp-default-method "ssh"
        tramp-default-user user-login-name
        tramp-default-host "localhost"))

;;; Imenu
(use-package imenu-list)

;;; Org
(use-package org
  :hook ((org-mode . visual-line-mode))
  :bind (("C-c l" . org-follow-location))
  :init
  (setq org-html-htmlize-output-type 'css
	org-html-head-include-default-style nil
	org-ascii-text-width 80
	org-attach-id-dir (join-paths company-cdn-url "media/")
	org-edit-src-content-indentation 0
	org-structure-template-alist '(("s" . "src")
				       ("e" . "src emacs-lisp")
				       ("x" . "src shell")
				       ("l" . "src lisp")
				       ("h" . "export html")
				       ("p" . "src python")
				       ("r" . "src rust")
				       ("E" . "example")
				       ("q" . "quote")
				       ("c" . "center")
				       ("C" . "comment")
				       ("v" . "verse"))
	org-babel-default-header-args '((:session . "none") (:results . "replace") 
					(:eval . "no-export") (:exports . "both")
					(:cache . "no") (:noweb . "no") 
					(:hlines . "no") (:tangle . "no"))
	org-global-properties '(quote 
				(("EFFORT_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
				 ("STYLE_ALL" . "habit"))))
  :config
  (org-babel-do-load-languages
   ;; TODO 2021-10-24: bqn, apl, k
   'org-babel-load-languages '((shell . t)
			       (emacs-lisp . t)
			       (lisp . t)
			       (org . t)
			       (eshell . t)
			       (calc . t)
			       (sed . t)
			       (awk . t)
			       (dot . t)
			       (js . t)
			       (C . t)
			       (python . t)))

  (defun org-mode-ask-effort ()
    "Ask for an effort estimate when clocking in."
    (unless (org-entry-get (point) "Effort")
      (let ((effort
	     (completing-read
	      "Effort: "
	      (org-entry-get-multivalued-property (point) "Effort"))))
	(unless (equal effort "")
	  (org-set-property "Effort" effort)))))

  (add-hook 'org-clock-in-prepare-hook
	    'org-mode-ask-effort)

  (with-eval-after-load "preview"
    '(add-to-list 'preview-default-preamble "\\PreviewEnvironment{circuitikz}" t))

  (setopt org-preview-latex-image-directory (join-paths user-emacs-directory ".cache/ltximg")
	  org-latex-image-default-width "8cm"
	  org-refile-use-cache t
	  org-refile-allow-creating-parent-nodes 'confirm

	  org-refile-targets '((org-agenda-files :maxlevel . 4))
	  ;; org-agenda-files (list "inbox.org")
	  org-agenda-include-diary t
	  org-agenda-include-inactive-timestamps t
	  org-agenda-span 5
	  org-confirm-babel-evaluate nil
	  org-src-fontify-natively t
	  org-src-tabs-act-natively t
	  org-footnote-section nil
	  org-log-into-drawer t
	  org-log-refile 'time
	  org-log-redeadline 'time
	  org-log-states-order-reversed nil
	  org-clock-persist 'history
	  org-clock-persist-file (join-paths user-emacs-directory "org-clock-save")
	  org-id-locations-file (join-paths user-emacs-directory "org-id-locations")))

(use-package org-crypt 
  :after (org)
  :ensure nil
  :autoload (org-crypt-use-before-save-magic))

(use-package org-agenda
  :after (org)
  :hook (hl-line-mode)
  :init
  (add-to-list 
   'org-agenda-custom-commands 
   '("i" "Work in progress tasks" ((todo "WIP") (agenda))) org-agenda-custom-commands)
  :config
  (defun org-agenda-reschedule-to-today ()
    (interactive)
    (cl-flet ((org-read-date (&rest rest) (current-time)))
      (call-interactively 'org-agenda-schedule)))

(defun org-agenda-current-subtree-or-region (only-todos)
  "Display an agenda view for the current subtree or region.
 With prefix, display only TODO-keyword items."
  (interactive "P")
  (let ((starting-point (point))
	header)
    (with-current-buffer (or (buffer-base-buffer (current-buffer))
			     (current-buffer))
      (if (use-region-p)
	  (progn
	    (setq header "Region")
	    (put 'org-agenda-files 'org-restrict (list (buffer-file-name (current-buffer))))
	    (setq org-agenda-restrict (current-buffer))
	    (move-marker org-agenda-restrict-begin (region-beginning))
	    (move-marker org-agenda-restrict-end
			 (save-excursion
			   ;; If point is at beginning of line, include
			   ;; heading on that line by moving forward 1.
			   (goto-char (1+ (region-end)))
			   (org-end-of-subtree))))
	;; No region; restrict to subtree.
	(save-excursion
	  (save-restriction
	    ;; In case the command was called from an indirect buffer, set point
	    ;; in the base buffer to the same position while setting restriction.
	    (widen)
	    (goto-char starting-point)
	    (setq header "Subtree")
	    (org-agenda-set-restriction-lock))))
      ;; NOTE: Unlike other agenda commands, binding `org-agenda-sorting-strategy'
      ;; around `org-search-view' seems to have no effect.
      (let ((org-agenda-sorting-strategy '(priority-down timestamp-up))
	    (org-agenda-overriding-header header))
	(org-search-view (if only-todos t nil) "*"))
      (org-agenda-remove-restriction-lock t)
      (message nil)))))

(use-package org-id
  :after (org))
(use-package org-protocol
  :after (org))
(use-package citeproc
  :after (org))

(use-package org-web-tools)
(use-package htmlize)
(use-package ol-notmuch)

;; (use-package auctex)

;;; Hexl
(use-package hexl
  :init (setq hexl-bits 8))

;;; Dictionary
(use-package dictionary
  :init (setq dictionary-server "dict.compiler.company"))

;;; Ispell
(use-package ispell
  :init ;; requires aspell and a hunspell dictionary (hunspell-en_us)
  (setq-default ispell-program-name "hunspell")
  :hook (mail-send-hook . ispell-message))

;;; Site Lisp
(use-package ulang
  :load-path user-emacs-site-lisp-directory
  :config (ulang-init))

(use-package scratch :load-path user-emacs-site-lisp-directory)

(use-package skel :load-path user-emacs-site-lisp-directory)
  
(use-package skt
  :load-path user-emacs-site-lisp-directory
  :after (skel))

(use-package graph :load-path user-emacs-site-lisp-directory)
(use-package inbox :load-path user-emacs-site-lisp-directory)
(use-package gen :load-path user-emacs-site-lisp-directory)
(use-package scrum :load-path user-emacs-site-lisp-directory)
(use-package mpk :load-path user-emacs-site-lisp-directory)

(provide 'default)
;; default.el ends here
