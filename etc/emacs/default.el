;;; default.el --- default config -*- lexical-binding:t -*-

;; Core Emacs Defaults

;;; Code:

;;; Defaults
(use-package emacs
  :hook (after-init . load-default-theme)
  :bind 
  (:map ctl-x-map
        ("C-b" . ibuffer)
        ("C-M-e" . eval-last-sexp))
  (:map ctl-x-r-map 
        ("SPC" . point-to-register)
        ("C-l" . list-registers)
        ("C-b" . buffer-to-register)
        ("C-f" . file-query-to-register)
        ("C-r" . copy-register))
  ("<remap> <tab-to-tab-stop>" . imenu))


;;; Icons
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
    "NOTMUCH_CONFIG"
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
  :defer nil
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
  :defer nil
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

;;; Elisp
(keymap-set emacs-lisp-mode-map "C-c C-l" #'load-file)
(keymap-set emacs-lisp-mode-map "C-c M-k" #'elisp-byte-compile-file)

;;; Lisp
(require 'slime-autoloads)
(use-package inf-lisp
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
  :after (cape)
  :autoload (slime slime-toggle slime-connect-file)
  :bind (:map slime-editing-map
              ("C-c s s" . slime-sprof-start)
              ("C-c s x" . slime-sprof-stop)
              ("C-c s r" . slime-sprof-report))
  :init
  (setq scheme-program-name "gsi"
	slime-auto-start t
	guile-program "guile"
	cmulisp-program "lisp"
	scsh-program "scsh"
	;; rebind the defpackage-regexp function to include DEFPKG
	slime-defpackage-regexp
	"^(\\(cl:\\|common-lisp:\\|uiop:\\|uiop/package:\\|std:\\|std/defpkg:\\|pkg:\\)?\\(defpackage\\|define-package\\|defpkg\\)\\>[ \t']*"
	slime-threads-update-interval 4
	slime-contribs '(slime-fancy
			 slime-quicklisp
			 slime-hyperdoc
			 ;; slime-listener-hooks
			 ;; slime-enclosing-context
			 ;; slime-media
			 ;; slime-mrepl
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
  :config
  (slime-setup slime-contribs)
  (slime-cape-enable)
  ;; fix slime repl mode
  (unbind-key "C-c C-d C-a" 'slime-repl-mode-map)
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
  (setq common-lisp-style-default "core"))

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
  :bind 
  ("C-c C-p" . outline-previous-heading)
  ("C-c C-n" . outline-next-heading)
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
;;; Conf
(with-eval-after-load 'rust-mode
  (use-package conf-mode
    :bind (:map conf-toml-mode-map
                ("C-c C-c C-r" . #'rust-run)
                ("C-c C-c C-u" . #'rust-compile)
                ("C-c C-c C-t" . #'rust-test))))

;;; Shell
(use-package shell
  :defer nil
  :config
  (defun set-no-process-query-on-exit ()
    (let ((proc (get-buffer-process (current-buffer))))
      (when (processp proc)
        (set-process-query-on-exit-flag proc nil))))
  :hook
  (shell-mode . set-no-process-query-on-exit)
  (term-exec . set-no-process-query-on-exit))

;;; Eshell
(use-package eshell
  :defer nil
  :init
  (setq eshell-highlight-prompt t
	eshell-hist-ignoredups t
	eshell-save-history-on-exit t
	eshell-prefer-lisp-functions nil
	eshell-destroy-buffer-when-process-dies t)
  :bind (:map eshell-mode-map ("C-d" . eshell-quit-or-delete-char))
  :config
  (require 'em-alias)
  (eshell/alias "d" "dired $1")
  (eshell/alias "ff" "find-file $1")

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
  :hook (eshell-mode . eat-eshell-mode)
  :init
  (setq eat-enable-auto-line-mode t
	eat-kill-buffer-on-exit t))

;;; Eww
(use-package shr
  :defer nil
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
  :autoload (eww)
  :hook ((eww-mode .shr-heading-setup-imenu)
	 (eww-mode . (lambda () )))
  :bind (:map eww-mode-map 
	      ("i" . shr-heading-map))
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
(use-package imenu-list :ensure t)

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

;;; MPC
(use-package mpc
  :bind
  (:map mpc-mode-map
    ("v" . mpc-tagbrowser)
    ("a" . mpc-playlist-add)
    ("c" . mpc-playlist-create)
    ("." . mpc-play-at-point)
    ("P" . mpc-resume)
    ("f" . mpc-ffwd)
    ("b" . mpc-rewind)
    ("x" . mpc-playlist-delete)
    ("m" . mpc-mark)
    ("1" . mpc-playlist))
  :config
  (defun mpc-mark ()
    "Mark mpc song at point and move to next line."
    (interactive)
    (mpc-select-toggle)
    (next-line)))

;;; Diary
(use-package diary-lib
  :init (setq diary-list-include-blanks t))

;;; Org
(use-package org
  :hook (org-mode-hook . visual-line-mode)
  :bind 
  (:map org-mode-map 
        ("C-c l" . org-follow-location)
        ("C-c t" . org-todo))
  :init
  (defun ol-vc-expand (tag)
    "Expand the tag of an org-link where linkkey is `vc'."
    (let ((f (split-string tag ":" "/")))
      (concat (string-trim-right company-vc-url "[/]")
	      (cl-case (length f)
		(0 "")
		(1 (format "/%s" (car f)))
		(2 (apply 'format "/%s/file/tip/%s" f))
		(t (apply 'format "/%s/file/%s/%s" f))))))
  (setq org-html-htmlize-output-type 'css
	org-html-head-include-default-style nil
	org-ascii-text-width 80
	org-man-command 'woman
        org-safe-remote-resources '("\\`https://cdn\\.compiler\\.company/org/clean\\.theme\\'")
        org-publish-timestamp-directory (join-paths user-emacs-directory ".org-timestamps/")
	org-attach-id-dir (join-paths company-cdn-url "media/")
	org-edit-src-content-indentation 0
	org-archive-location "archive.org::"
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
	org-link-abbrev-alist `(("vc" . ol-vc-expand)
				("comp" . ,(format "https://%s/%%s" company-domain))
				("cdn" . ,(format "%s/%%s" company-cdn-url))
				("packy" . ,(format "%s/%%s" company-packy-url))
				("yt" . "https://youtube.com/watch?v=%s")
				("gh" . "https://github.com/%s")
				("cb" . "https://codeberg.org/%s")
				("wikipedia" . "https://en.wikipedia.org/wiki/%s")
				("archwiki" . "https://wiki.archlinux.org/title/%s")
				("reddit" . "https://reddit.com/%s")
				("hn" . "https://news.ycombinator.com/%s")
				("archive" . "https://web.archive.org/web/%s")
				("so" . "https://stackoverflow.com/%s"))
	org-babel-default-header-args '((:session . "none") (:results . "replace") 
					(:eval . "no-export") (:exports . "both")
					(:cache . "no") (:noweb . "no") 
					(:hlines . "no") (:tangle . "no"))
	org-global-properties '(quote 
				(("EFFORT_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
				 ("STYLE_ALL" . "habit")))
	org-todo-keywords '((sequence "TBD(0!)" "TODO(t!)" "NEXT(n!)" "WIP(i!)" "|" "DONE(d!)")
			    (sequence "HOLD(H@/!)" "WIP(!)" "|")
			    (sequence "WAIT(W@/!)" "WIP(!)" "|")
			    (sequence "RESEARCH(s!)" "WIP(!)" "REPORT(c!)" "|")
			    (sequence "OUTLINE(O!)" "DRAFT(M!)" "REVIEW(V!)" "|")
			    (sequence "FIXME(f!)" "WIP(!)" "TEST(T!)" "|")
			    (type "FIND(q!)" "READ(r@!)" "WATCH(A@!)" "HACK(h!)"
				  "CODE(c!)" "BENCH(b!)" "DEPLOY(D!)" "RUN(X!)"
				  "REFILE(w!)" "LOG(L!)" "GOTO(g!)" "|")
			    (type "PROJECT(p!)" "PRODUCT(P!)" "SPRINT(S!)" "RELEASE(R!)" "|")
			    (sequence "|" "DONE(d!)" "NOPE(x@!)"))
	org-todo-keyword-faces '(("PROJECT" . (:foreground "lightseagreen" :weight bold))
				 
				 ("PRODUCT" . (:foreground "olivedrab" :weight bold))
				 ("RELEASE" . (:foreground "maroon3" :weight bold))
				 ("RESEARCH" . (:foreground "maroon2" :weight bold))
				 ("HACK" . (:foreground "maroon3" :weight bold))
				 ("TBD" . (:foreground "brown" :weight bold))
				 ("CODE" . (:foreground "bisque" :weight bold :background "midnightblue"))
				 ("HOLD" . (:foreground "red1" :weight bold :background "yellow1"))
				 ("WAIT" . (:foreground "red4" :weight bold :background "yellow1"))
				 ("WIP" . (:foreground "darkorchid2" :weight bold))
				 ("NOPE" . (:foreground "hotpink" :weight bold :background "darkgreen")))
	org-stuck-projects '("+PROJECT/-DONE" ("NEXT") nil ""))
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
	  org-default-notes-file (join-paths org-directory "inbox.org")
	  org-refile-targets '((org-agenda-files :maxlevel . 4))
	  ;; org-agenda-files (list "inbox.org")
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

(use-package org-capture
  :after (org)
  :init 
  (setq org-capture-use-agenda-date t
	org-capture-templates 
    '(("1" "current-task-item" item (clock) "%i%?")
	  ("2" "current-task-checkbox" checkitem (clock) "%i%?")
	  ("3" "current-task-region" plain (clock) "%i" :immediate-finish t :empty-lines 1)
	  ("4" "current-task-kill" plain (clock) "%c" :immediate-finish t :empty-lines 1))))

(use-package org-crypt 
  :after (org)
  :ensure nil
  :autoload (org-crypt-use-before-save-magic))

(use-package org-agenda
  :after (org)
  :hook (hl-line-mode)
  :bind ("C-c a" . org-agenda)
  :init
  (setq org-agenda-include-diary t
        org-agenda-include-inactive-timestamps t
        org-agenda-span 7)
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
  :after (org)
  :hook (kill-emacs-hook . org-id-locations-save)
  :init (setq org-id-link-to-org-use-id t))

(use-package org-protocol
  :after (org)
  :init (setq org-protocol-default-template-key "L"
	      org-protocol-project-alist
	      '(("comp"
		 :base-url company-url
		 :working-directory company-org-directory
		 :online-suffix ".html"
		 :working-suffix ".org"))))

(use-package citeproc
  :ensure t
  :defer t
  :after (org))

(use-package org-expire
  :load-path user-emacs-site-lisp-directory
  :after (org org-id)
  :hook (org-after-todo-state-change-hook . (org-expire-insert-created org-id-get-create)))

(use-package org-web-tools
  :ensure t
  :after (org))

(use-package htmlize
  :ensure t
  :after (org))

(use-package ol-notmuch
  :ensure t
  :after (org))

;; (use-package auctex)

;;; Hexl
(use-package hexl
  :init (setq hexl-bits 8))

;;; Desktop
(use-package desktop
  :config
  (setopt desktop-auto-save-timeout 60
          desktop-base-file-name ".desktop"
          desktop-base-lock-name ".desktop.lock")
  (add-to-list 'desktop-path "."))

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
  :bind-keymap 
  ("C-c (" . parens-map)
  ("<XF86Paste>" . parens-map)
  ("C-c c" . user-map)
  :hook (after-init-hook . load-keys)
  :load-path user-emacs-site-lisp-directory)

(use-package scratch 
  :defer nil
  :load-path user-emacs-site-lisp-directory)

(use-package organ 
  :defer nil
  :load-path user-emacs-site-lisp-directory
  :hook (org-after-todo-state-change . org-clock-in-wip))

(use-package graph 
  :load-path user-emacs-site-lisp-directory
  :hook (org-mode . org-graph-maybe-enable))

(use-package inbox
  :defer nil
  :load-path user-emacs-site-lisp-directory
  :after (org-expire)
  :config (load-org-inbox-capture-templates))

(use-package gen 
  :load-path user-emacs-site-lisp-directory
  :mode ("\\.gen" . lisp-mode)
  :hook (lisp-mode . maybe-enable-gen-minor-mode))

(use-package plan 
  :load-path user-emacs-site-lisp-directory
  :defer nil
  ;; used in org/meta/babel.org, called via org-dblocks in project
  ;; readmes.
  :ensure-system-package tokei)

(use-package skel 
  :load-path user-emacs-site-lisp-directory
  :interpreter ("skel" . skel-mode)
  :hook (common-lisp-lisp-mode-hook . organ-minor-mode))

(use-package skt
  :load-path user-emacs-site-lisp-directory
  :after (skel))

(use-package mpk 
  :load-path user-emacs-site-lisp-directory)

(provide 'default)
;; default.el ends here
