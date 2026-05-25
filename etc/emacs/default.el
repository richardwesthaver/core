;;; default.el --- default config -*- lexical-binding:t -*-

;; Core Emacs Defaults

;;; Code:

;;; UI
(use-package hide-mode-line
  :hook (speedbar-mode . hide-mode-line-mode))

;;;; Icons
;; all-the-icons all-the-icons-dired all-the-icons-ibuffer ;; icons
(use-package nerd-icons :ensure t)
(use-package nerd-icons-ibuffer :hook (ibuffer-mode . nerd-icons-ibuffer-mode) :after (ibuffer) :ensure t)
(use-package nerd-icons-dired :hook (dired-mode . nerd-icons-dired-mode) :after (dired) :ensure t)
(use-package nerd-icons-grep :hook (grep-mode . nerd-icons-grep-mode) :ensure t)
(use-package nerd-icons-xref :hook (xref-mode . nerd-icons-xref-mode) :ensure t)
(use-package tab-line-nerd-icons :hook (tab-line-mode . tab-line-nerd-icons-global-mode) :ensure t)

;;; Whitespace
(use-package whitespace
  :init
  (setq
   whitespace-style '(face tabs trailing lines-tail indentation::space)
   whitespace-line-column 88))

;;; Env
(use-package exec-path-from-shell
  :ensure t
  :init
  (add-to-list 'exec-path "/usr/bin/")
  (add-to-list 'exec-path "/usr/sbin/")
  (add-to-list 'exec-path "/usr/local/bin/")
  (add-to-list 'exec-path "/usr/local/share/lisp/")
  (add-to-list 'exec-path "/usr/share/lisp/")
  (add-to-list 'exec-path (join-paths user-home-directory ".local/bin/"))
  :config
  (exec-path-from-shell-copy-envs 
   (list 
    "SSH_AGENT_PID"
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

(use-package cape :ensure t)

;; (use-package consult)

(use-package marginalia :ensure t)

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

(use-package company :ensure t)

(use-package expand
  :hook ((expand-expand . indent-according-to-mode)
	     (expand-jump . indent-according-to-mode)))

(use-package completion-preview
  :ensure t
  :config (global-completion-preview-mode))

(use-package corfu
  :ensure t
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
         (completion-in-region beg end table pred)))))
  (keymap-set corfu-map "M-m" #'corfu-move-to-minibuffer)
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer))

(use-package nerd-icons-corfu
  :ensure t
  :after (corfu)
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-completion 
  :ensure t
  :hook (completion-mode . nerd-icons-completion-mode))

;;; Dired
(use-package dired
  :config
  (setq dired-dwim-target t
	    dired-free-space 'separate
	    dired-free-space nil
	    dired-mouse-drag-files t)
  (when (linux-p) (setq dired-listing-switches "-alsh")))

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
(require 'slime-autoloads)
(use-package inf-lisp
  :defer nil
  :init 
  (setq inferior-lisp-program
	    (format "%s --dynamic-space-size=8G --control-stack-size=32"
		        (if (file-exists-p "/bin/core") "/bin/core" "/bin/sbcl")))
  (defvar lisp-toggle nil)  
  :config
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
	    (inferior-lisp (or cmd inferior-lisp-program))))))

(use-package slime
  :defer nil
  :after (company cape inf-lisp)
  :hook 
  (lisp . (slime-mode slime-cape-enable))
  (inferior-lisp . inferior-slime-mode)
  (slime-repl . slime-cape-enable)
  :init
  (defvar slime-toggle nil)
  :config
  (setq scheme-program-name "gsi"
	    slime-auto-start t
	    guile-program "guile"
	    cmulisp-program "lisp"
	    scsh-program "scsh"
	    ;; rebind the defpackage-regexp function to include DEFPKG
	    slime-defpackage-regexp
	    "^(\\(cl:\\|common-lisp:\\|uiop:\\|uiop/package:\\|std:\\|std/defpkg:\\|pkg:\\)?\\(defpackage\\|define-package\\|defpkg\\)\\>[ \t']*"
	    common-lisp-style-default "core"
	    slime-threads-update-interval 1
	    
	    slime-contribs '(slime-fancy
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
			             slime-cl-indent
			             ;; slime-snapshot
			             slime-sprof
			             slime-tramp
			             ;; slime-typeout-frame
			             slime-xref-browser
			             ;; slime-highlight-edits
			             slime-repl-ansi-color))
  (slime-setup slime-contribs)
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
  (defun slime-connect-file (file &optional host)
    "Connect to the port number stored in FILE which should be the same value
as the first argument to SWANK:START-SERVER on the Lisp side."
    (interactive "fswank file: ")
    (slime-connect
     (or host "localhost")
     (string-to-number
      (with-temp-buffer
	    (insert-file-contents file)
	    (buffer-string)))))
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
    (defsclass (as defclass)))))

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

;;; Terminal
(use-package eat
  :ensure t
  :init
  (setq eat-enable-auto-line-mode t
	eat-kill-buffer-on-exit t)
  :config
  (eat-eshell-mode))

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
        tramp-default-host "localhost"
        tramp-auto-save-directory (expand-file-name "auto-save/tramp/" user-emacs-directory)))

;;; Imenu
(use-package imenu-list)

;;; Calc
(use-package calc
  :init
  (setq calc-highlight-selections-with-faces t)
  :config
  (cl-pushnew (cons 'lisp-mode (list "#| " "|#
")) calc-embedded-open-close-mode-alist)
  (cl-pushnew '(emacs-lisp-mode ";; " "
") calc-embedded-open-close-mode-alist)

  (defun calc-eval-region (arg beg end)
    "Calculate the region and display the result in the echo area.
With prefix ARG non-nil, insert the result at the end of region."
    (interactive "P\nr")
    (let* ((expr (buffer-substring-no-properties beg end))
	   (result (calc-eval expr)))
      (if (null arg)
	  (message "%s = %s" expr result)
	(goto-char end)
	(save-excursion
	  (insert result)))))

  (defun calc-embedded-formula-to-stack ()
    (interactive)
    (save-excursion
      (save-match-data
	(calc-embedded-find-bounds)))
    (let ((eq-str (buffer-substring calc-embed-top calc-embed-bot)))
      (calc-eval eq-str 'push))))

;;; Diary
(use-package diary-lib
  :init (setq diary-list-include-blanks t))

;;; Org
(use-package org
  :hook ((org-mode . visual-line-mode))
  :bind (("C-c l" . org-follow-location))
  :init
  (setq org-html-htmlize-output-type 'css
	    org-html-head-include-default-style nil
	    org-ascii-text-width 80
        org-safe-remote-resources '("\\`https://cdn\\.compiler\\.company/org/clean\\.theme\\'")
        org-publish-timestamp-directory (join-paths user-emacs-directory ".org-timestamps/")
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
  :config
  (add-to-list 
   'org-agenda-custom-commands 
   '("i" "Work in progress tasks" ((todo "WIP") (agenda))) org-agenda-custom-commands)
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
(use-package org-expiry
  :load-path user-emacs-site-lisp-directory
  :after (org))
(use-package org-web-tools
  :ensure t
  :after (org))
(use-package htmlize
  :ensure t
  :after (org))
(use-package ol-notmuch
  :ensure t
  :after (org))

(use-package ox
  :config
  ;; TODO 2025-10-08: 
  (defun org-html-format-drawer (name contents)
    "Default function used as value for `org-html-format-drawer-function'."
    (let ((name (downcase name)))
      (format "<details class='edges'><summary>%s</summary>%s</details>"
	          name
	          (pcase name
		        ("edges"
		         (unless (null contents)
		           (let ((es (intersperse "<br>" (s-lines contents))))
		             (if (> (length es) 3)
			             (progn
			               (setf (cadr es) nil
				                 (nth (1- (length es)) es) nil)
			               (apply 'concat (flatten es)))
		               (apply 'concat es)))))
		        (_ contents)))))

  ;; replace hardcoded value
  (defun org-html-property-drawer (_drawer contents _info)
    "Transcode a PROPERTY-DRAWER element from Org to HTML.
CONTENTS holds the contents of the drawer.  INFO is a plist holding
contextual information."
    (format "<details class='properties'><summary>props</summary>\n%s</details>" (apply 'concat (intersperse "<br>" (s-lines contents)))))

  (defun org-export-get-reference-title (datum info)
    "Like `org-export-get-reference', except uses heading titles instead of random numbers."
    (let ((cache (plist-get info :internal-references)))
      (or (car (rassq datum cache))
          (let* ((crossrefs (plist-get info :crossrefs))
		         (cells (org-export-search-cells datum))
		         ;; Preserve any pre-existing association between
		         ;; a search cell and a reference, i.e., when some
		         ;; previously published document referenced a location
		         ;; within current file (see
		         ;; `org-publish-resolve-external-link').
		         ;;
		         ;; However, there is no guarantee that search cells are
		         ;; unique, e.g., there might be duplicate custom ID or
		         ;; two headings with the same title in the file.
		         ;;
		         ;; As a consequence, before re-using any reference to
		         ;; an element or object, we check that it doesn't refer
		         ;; to a previous element or object.
		         (new (or (cl-some
                           (lambda (cell)
                             (let ((stored (cdr (assoc cell crossrefs))))
                               (when stored
				                 (let ((old (org-export-format-reference stored)))
                                   (and (not (assoc old cache)) stored)))))
                           cells)
                          (when (org-element-property :raw-value datum)
                            ;; Heading with a title
                            (org-export-new-title-reference datum cache))
                          ;; NOTE: This probably breaks some Org Export
                          ;; feature, but if it does what I need, fine.
                          (org-export-format-reference
                           (org-export-new-reference cache))))
		         (reference-string new))
            ;; Cache contains both data already associated to
            ;; a reference and in-use internal references, so as to make
            ;; unique references.
            (dolist (cell cells) (push (cons cell new) cache))
            ;; Retain a direct association between reference string and
            ;; DATUM since (1) not every object or element can be given
            ;; a search cell (2) it permits quick lookup.
            (push (cons reference-string datum) cache)
            (plist-put info :internal-references cache)
            reference-string))))

  (defun org-export-new-title-reference (datum cache)
    "Return new reference for DATUM that is unique in CACHE."
    (cl-macrolet ((inc-suffixf (place)
                    `(progn
                       (string-match (rx bos
					                     (minimal-match (group (1+ anything)))
					                     (optional "--" (group (1+ digit)))
					                     eos)
                                     ,place)
                       ;; HACK: `s1' instead of a gensym.
                       (-let* (((s1 suffix) (list (match-string 1 ,place)
                                                  (match-string 2 ,place)))
                               (suffix (if suffix
                                           (string-to-number suffix)
					                     0)))
                              (setf ,place (format "%s--%s" s1 (cl-incf suffix)))))))
      (let* ((title (org-element-property :raw-value datum))
             (ref (url-hexify-string (substring-no-properties title)))
             (parent (org-element-property :parent datum)))
	    (while (--any (equal ref (car it))
                      cache)
          ;; Title not unique: make it so.
          (if parent
              ;; Append ancestor title.
              (setf title (concat (org-element-property :raw-value parent)
                                  "--" title)
                    ref (url-hexify-string (substring-no-properties title))
                    parent (org-element-property :parent parent))
            ;; No more ancestors: add and increment a number.
            (inc-suffixf ref)))
	    ref)))

  (defun org-html--reference (datum info &optional named-only)
    "Return an appropriate reference for DATUM.
DATUM is an element or a `target' type object.  INFO is the
current export state, as a plist.
When NAMED-ONLY is non-nil and DATUM has no NAME keyword, return
nil.  This doesn't apply to headlines, inline tasks, radio
targets and targets."
    (let* ((type (org-element-type datum))
	       (user-label
	        (org-element-property
	         (pcase type
	           ((or `headline `inlinetask) :CUSTOM_ID)
	           ((or `radio-target `target) :value)
	           (_ :name))
	         datum))
	       (user-label (or user-label
			               (when-let* ((path (org-element-property :ID datum)))
			                 path))))
      (cond
       ((and user-label
	         (or (plist-get info :html-prefer-user-labels)
		         ;; Used CUSTOM_ID property unconditionally.
		         (memq type '(headline inlinetask))))
	    user-label)
       ((and named-only
	         (not (memq type '(headline inlinetask radio-target target)))
	         (not user-label))
	    nil)
       (t
	    (org-export-get-reference datum info)))))
  
  (define-minor-mode org-id-export-mode
    "Attempt to export Org as HTML with useful link IDs.
Instead of random IDs like \"#orga1b2c3\", use heading titles, made
unique when necessary."
    :global t
    (if org-id-export-mode
        (advice-add #'org-export-get-reference :override #'org-export-get-reference)
      (advice-remove #'org-export-get-reference #'org-export-get-reference))))

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

;;; Core Extensions
(use-package ulang
  :defer nil
  :load-path user-emacs-site-lisp-directory
  :config (ulang-init))

(use-package keymaps 
  :defer nil
  :load-path user-emacs-site-lisp-directory)

(use-package scratch :load-path user-emacs-site-lisp-directory)
(use-package organ :load-path user-emacs-site-lisp-directory)
(use-package graph :load-path user-emacs-site-lisp-directory)
(use-package inbox 
  :load-path user-emacs-site-lisp-directory
  :after (org-expiry))
(use-package gen :load-path user-emacs-site-lisp-directory)
(use-package scrum :load-path user-emacs-site-lisp-directory)
(use-package skel :load-path user-emacs-site-lisp-directory)
(use-package skt
  :load-path user-emacs-site-lisp-directory
  :after (skel))
(use-package mpk :load-path user-emacs-site-lisp-directory)

(provide 'default)
;; default.el ends here
