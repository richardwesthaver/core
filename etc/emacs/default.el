;;; default.el --- default config -*- lexical-binding:t -*-

;; Core Emacs Defaults

;;; Code:
(require 'site-start)

;;; Utils
(defmacro mapadd (x y &optional append test)
  `(setf ,x (cl-remove-duplicates (append ,x ,y) :test 'equal :from-end t)))

(defun add-to-load-path (&rest paths)
  "Add PATHS to `load-path'."
  (mapadd load-path paths))

(defun darwin-p () (string= system-type "darwin"))
(defun linux-p () (string= system-type "gnu/linux"))

(defun join-paths (root &rest dirs)
  "helper function for joining strings to a path."
  (let ((result root))
    (cl-loop for dir in dirs do
             (setq result (concat (file-name-as-directory result) dir)))
    result))

(defun mkstr (&rest args)
  "Paul Graham's 'mkstr' utility from On Lisp.

Coerce ARGS into a single string and return it."
  (let* ((s ""))
    (dolist (a args)
      (cond
       ((null a) nil)
       ((sequencep a) (setq s (concat s a)))
       ((numberp a) (setq s(concat s (number-to-string a))))
       ((symbolp a) (setq s(concat s (symbol-name a))))))
    s))

(defun symb (&rest args)
  "Concat ARGS and return a newly interned symbol."
  (intern (apply #'mkstr args)))

(defun group (source n)
  "Group a list of arguments SOURCE by any provided grouping amount N.

For example:
(group (quote (foo 2 bar 4)) 2) ;=> ((foo 2) (bar 4))
(group (quote (a b c d e f)) 3) ;=> ((a b c) (d e f))"
  (when (zerop n) (error "zero length"))
  (cl-labels ((rec (source acc)
                (let ((rest (nthcdr n source)))
                  (if (consp rest)
                      (rec rest (cons
                                 (cl-subseq source 0 n)
                                 acc))
                    (nreverse
                     (cons source acc))))))
    (when source (rec source nil))))

(defun flatten (x)
  "Given a tree X, return all the leaves of the tree."
  (cl-labels ((rec (x acc)
                (cond ((null x) acc)
                      ((atom x) (cons x acc))
                      (t (rec
                          (car x)
                          (rec (cdr x) acc))))))
    (rec x nil)))

(defun intersperse (element list)
  "Intersperse ELEMENT between each element of LIST."
  (if (null list)
      nil
    (cons (car list)
          (cl-mapcan (lambda (x) (list element x)) (cdr list)))))

(defun hash-table-alist (table)
  "Returns an association list containing the keys and values of hash table
TABLE."
  (let ((alist nil))
    (maphash (lambda (k v)
               (push (cons k v) alist))
             table)
    (nreverse alist)))

(defun load-default-theme (&optional theme)
  (interactive)
  (when theme (setq default-theme theme))
  (load-theme default-theme t))

(defun load-keys (&optional custom)
  (let ((keydefs (or custom "keymaps")))
    (load keydefs nil t)))

(defun gen-site-lisp-autoloads (&optional output)
  (interactive)
  (loaddefs-generate 
   (list
    site-lisp-directory
    (join-paths site-lisp-directory "slime"))
   (or output"autoloads.el")))

(defun gen-lisp-autoloads ()
  (interactive)
  (loaddefs-generate 
   user-lisp-directory
   (join-paths user-lisp-directory "autoloads.el")))

(defun upgrade-emacs (&optional ask)
  (interactive)
  (package-refresh-contents)
  (package-install-selected-packages (not ask))
  (package-upgrade-all ask))

;;; Defaults
(use-package emacs
  :defer nil
  :hook 
  (after-init . load-default-theme)
  (Buffer-menu-mode . hl-line-mode)
  (ibuffer-mode . hl-line-mode)
  :bind-keymap 
  ("C-c t" . clock-map)
  ("C-c r" . review-map)
  ("C-c SPC" . toggle-map)
  ("C-c ." . status-map)
  ("C-c s" . search-map)
  :bind 
  (:map ctl-x-map
        ("C-b" . ibuffer)
        ("C-M-e" . eval-last-sexp)
        ("C-M-n" . next-buffer)
        ("C-M-p" . previous-buffer)
        ("n" . duplicate-dwim))
  (:map ctl-x-r-map 
        ("SPC" . point-to-register)
        ("C-l" . list-registers)
        ("C-b" . buffer-to-register)
        ("C-f" . file-query-to-register)
        ("C-r" . copy-register))
  (:map help-map
        ("K" . describe-keymap))
  ("<remap> <tab-to-tab-stop>" . imenu)
  ([remap dabbrev-expand] . hippie-expand)
  :config
  (add-to-load-path 
   user-lisp-directory
   lisp-directory
   site-lisp-directory 
   (expand-file-name "slime" site-lisp-directory))
  ;; Load autoloads if they exist
  (require 'autoloads (expand-file-name "autoloads" site-lisp-directory) t))

;; use-package defaults
(use-package system-packages :ensure t)
(use-package diminish :ensure t)
(use-package delight :ensure t)
(setq use-package-expand-minimally t)

;;; Icons
;; all-the-icons all-the-icons-dired all-the-icons-ibuffer
(use-package nerd-icons :ensure t)
(use-package nerd-icons-ibuffer :hook (ibuffer-mode . nerd-icons-ibuffer-mode) :after (ibuffer) :ensure t)
(use-package nerd-icons-grep :hook (grep-mode . nerd-icons-grep-mode) :ensure t)
(use-package nerd-icons-xref :hook (xref-mode . nerd-icons-xref-mode) :ensure t)

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
  (mapadd exec-path
          `("/usr/bin/"
            "/usr/sbin/"
            "/usr/local/bin/"
            "/usr/local/share/lisp/"
            "/usr/share/lisp/"
            ,(join-paths user-home-directory ".local/bin/")))
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
  :defer nil
  :config (vertico-mode 1)
  :bind 
  (:map vertico-map 
        ("M-q" . #'vertico-quick-insert)
        ("C-M-q" . #'vertico-quick-exit)))

(use-package orderless
  :defer nil
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
  :defer nil
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
  (mapadd completion-at-point-functions '(cape-dabbrev cape-abbrev cape-file))
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

;;; Time
(use-package time
  :bind (:map status-map ("." . world-clock)))

;;; Dired
(use-package dired
  :hook (dired-mode . image-dired-minor-mode)
  :after (vc)
  :init
  (setq dired-dwim-target t
	    dired-free-space 'separate
	    dired-mouse-drag-files t)
  (when (linux-p) (setq dired-listing-switches "-alsh")))

(use-package nerd-icons-dired :hook (dired-mode . nerd-icons-dired-mode) :after (dired) :ensure t)

;;; Speedbar
(use-package speedbar
  :defer nil
  :config
  (setq speedbar-sort-tags t
	    speedbar-prefer-window t
	    speedbar-track-mouse-flag t)
  (add-to-list 'speedbar-obj-alist '("\\.lisp$" . ".fasl"))
  (add-to-list 'speedbar-obj-alist '("\\.sys$" . ".fsys")))

;;; Tempo
(use-package tempo
  :config
  (defun tempo-tags-variable (mode)
    "Return a tempo tags variable's symbol for MODE."
    (when mode
      (intern (replace-regexp-in-string
               (rx "-mode" eos) "-tempo-tags"
               (symbol-name mode))))))

;;; TODO Skeleton
(use-package skeleton)

;;; Projects
(use-package project
  :config
  (setopt project-list-file (expand-file-name "projects" user-emacs-directory)
          project-mode-line t
          project-file-history-behavior 'relativize)
  (defun remember-project ()
    (interactive)
    (project-remember-project (project-current))
    project--list))

;;; Tabs
(use-package tab-bar
  :hook (tab-bar-mode-hook . tab-bar-history))

(use-package tab-line-nerd-icons :ensure t :hook (tab-line-mode . tab-line-nerd-icons-global-mode))

;;; Ulang
(use-package ulang
  :defer nil
  :bind (:map org-mode-map ("C-c L" . org-follow-location))
  :load-path site-lisp-directory
  :hook 
  (after-init . ulang-init)
  (lisp-mode . org-minor-mode)
  (lisp-mode . ulang--lisp-page-delimiter)
  (org-mode . ulang--org-page-delimiter)
  (sh-script-mode . ulang--sh-page-delimiter))

;;; Eglot
(use-package eglot
  :bind (:map status-map ("e" . eglot-list-connections)))
;;; Lisp
(use-package lisp-mode
  :mode ("\\.sys\\'" "\\.gen\\'")
  :bind 
  ("<XF86Paste>" . lisp-mode-shared-map)
  (:map emacs-lisp-mode-map
        ("C-c C-l" . load-file)
        ("C-c M-k" . elisp-byte-compile-file))
  (:map lisp-mode-shared-map
        ("C-M-;" . prog-comment-dwim)
        ("C-c C-;" . prog-comment-timestamp-keyword)
        ("C-M-f" . forward-sexp)
        ("C-M-b" . backward-sexp)
        ("C-M-d" . down-list)
        ("C-M-u" . up-list)
        ("C-M-p" . backward-list)
        ("C-M-n" . forward-list)
        ("C-M-k" . kill-sexp)
        ("C-M-q" . indent-sexp)
        ("C-M-t" . transpose-sexps)
        ("C-M-r" . raise-sexp)
        ("C-M-c" . check-parens)
        ("C-M-x" . eval-defun)))

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
  :after (company cape lisp-mode)
  :autoload (slime slime-toggle slime-connect-file define-common-lisp-style)
  :bind 
  (:map slime-editing-map
        ("C-c s s" . slime-sprof-start)
        ("C-c s x" . slime-sprof-stop)
        ("C-c s r" . slime-sprof-report))
  (:map status-map
        ("l" . slime-list-connections)
        ("n" . slime-list-compiler-notes)
        ("S" . slime-list-all-repl-shortcuts)
        ("s" . slime-list-threads))
  :hook (slime-mode . (lambda () (completion-preview-mode -1)))
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
  (slime-setup slime-contribs)
  :config
  (slime-cape-enable)
  ;; fix slime repl mode
  (unbind-key "C-c C-d C-a" 'slime-repl-mode-map)
  (require 'lisp-style)
  (setq common-lisp-style-default "core"))

;;; Asm
(use-package nasm-mode
  :hook (asm . nasm-mode))

;;; Rust

(use-package rust-mode 
  :ensure t
  :after (conf-mode)
  :hook eglot-ensure
  :bind (:map conf-toml-mode-map
              ("C-c C-c C-r" . #'rust-run)
              ("C-c C-c C-u" . #'rust-compile)
              ("C-c C-c C-t" . #'rust-test))
  :init
  (setq rust-rustfmt-switches nil
	    rust-indent-offset 2))

;;; Python
(use-package python
  :hook eglot-ensure
  :init (setq python-indent-offset 2))

;;; Web
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
  (defun file-query-to-register (register &optional delete)
    (interactive
     (list
      (register-read-with-preview "File query to register: ")))
    (set-register register (list 'file-query (buffer-file-name (current-buffer)) (point))))
  (defun save-registers (&optional filename queryp)
    "Save the contents of all registers to a file as loadable data. Cannot
save window/frame configurations."
    (interactive "i\nP")
    (setf filename (if queryp (read-file-name nil registers-file) (or filename registers-file)))
    (let ((print-level nil)
          (print-length nil)
          (b (generate-new-buffer "*registers*")))
      (set-buffer b)
      (insert (format ";;; %s -*- mode:emacs-lisp; lexical-binding:t -*-\n" (file-name-base filename)))
      (dolist (i register-alist)
        (let ((char (car i))
              (contents (cdr i)))
          (cond
           ((or (stringp contents) (numberp contents))
            (insert (format "%S\n" `(set-register ,char ,contents))))
           ((markerp contents)
            (insert (format "%S\n" `(set-register ,char
                                                  '(file-query
                                                    ,(buffer-file-name (marker-buffer contents))
                                                    ,(marker-position contents))))))
           ((bufferp (cdr contents))
            (insert (format "%s\n" `(set-register ,char ',(buffer-name (cdr contents))))))
           (t (when (and contents
                         (not (or (window-configuration-p (car contents))
                                  (frame-configuration-p (car contents)))))
                (insert (format "%s\n" `(set-register ,char ',contents))))))))
      (delete-file filename)
      (write-file filename)
      (kill-buffer b))))

;;; Outlines
(use-package outline
  :defer nil
  :hook (view-mode . (lambda () (setq-local outline-minor-mode-use-buttons 'insert)))
  :bind 
  ("C-c C-p" . outline-previous-heading)
  ("C-c C-n" . outline-next-heading)
  ("C-c TAB" . outline-cycle)
  ("C-c <backtab>" . outline-cycle-buffer)
  :config
  (defun add-outline-hook (mode &optional rx buttons)
    (let ((sym (symb mode "-hook"))
          (body `(,@(when rx `((setq-local outline-regexp ,rx)))
                  ,@(when buttons `((setq-local outline-minor-mode-use-buttons ,buttons)))
                  (outline-minor-mode 1))))
      (print body)
      (add-hook sym `(lambda () ,@body))))

  (defmacro outline-hooks (&rest pairs)
    `(mapc (lambda (x) (apply 'add-outline-hook x)) ',pairs))

  (outline-hooks (asm-mode ";;;+")
		         (nasm-mode ";;;+")
		         (rust-mode "\\(//!\\|////+\\)")
		         (sh-mode "###+")
		         (sh-script-mode "###+")
		         (makefile-mode "###+")
		         (conf-mode "###+")
                 (fundamental-mode "###+")
                 (org-mode)
                 (prog-mode nil 'in-margins)))

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
  :bind 
  ("C-c RET" . eshell)
  ("C-c C-RET" . eshell-new)
  (:map eshell-mode-map ("C-d" . eshell-quit-or-delete-char))
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
  :after (imenu)
  :init
  (setq shr-use-colors t
	    shr-use-fonts t
	    shr-max-image-proportion 0.6
	    shr-image-animate t
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
  :hook shr-heading-setup-imenu
  ;; :bind (:map eww-mode-map ("i" . shr-heading-map))
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
(use-package imenu
  :bind ("C-c i" . imenu))

(use-package imenu-list :ensure t
  :bind ("C-c M-i" . imenu-list))

;;; Calc
(use-package calc
  :init
  (setq calc-highlight-selections-with-faces t)
  :config
  (cl-pushnew '(lisp-mode "#| " "|#") calc-embedded-open-close-mode-alist)
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
  ("C-c e p" . mpc)
  :config
  (defun mpc-mark ()
    "Mark mpc song at point and move to next line."
    (interactive)
    (mpc-select-toggle)
    (next-line)))

;;; Diary
(use-package diary-lib
  :config 
  (setopt diary-list-include-blanks t
          diary-file (join-paths org-directory "diary")))

;;; Remember
(use-package remember
  :defer nil
  :bind 
  ("C-x M-r" . remember)
  ("C-x C-M-r" . remember-notes)
  :init
  (setopt remember-data-file (join-paths org-directory "notes")
          remember-mailbox (join-paths user-mail-directory "remember")
          remember-initial-major-mode 'org-mode))

;;; Org
(use-package org
  :hook 
  (org-mode . visual-line-mode)
  (org-clock-in-prepare . org-mode-ask-effort)
  :bind 
  ("C-c l" . org-store-link)
  ("C-c c" . org-capture)
  (:map org-mode-map ("C-c t" . org-todo))
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
				                  "REFILE(w!)" "LOG(L!)" "GET(g!)" "GOTO(G!)" "PRACTICE(a!)" "|")
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
  (setopt 
   ;; org-preview-latex-image-directory (join-paths user-emacs-directory ".cache/ltximg")
   ;; org-latex-image-default-width "8cm"
   org-refile-use-cache t
   org-refile-use-outline-path 'full-file-path
   org-outline-path-complete-in-steps nil
   org-refile-allow-creating-parent-nodes 'confirm
   org-default-notes-file (join-paths org-directory "inbox.org")
   org-refile-targets `((org-agenda-files :maxlevel . 4))
   ;; org-agenda-files (list "inbox.org")
   org-confirm-babel-evaluate nil
   org-src-fontify-natively t
   org-src-tabs-act-natively t
   org-footnote-section nil
   org-log-into-drawer t
   org-log-refile 'time
   org-log-redeadline 'time
   org-log-states-order-reversed nil
   org-clock-persist t
   org-clock-persist-file (join-paths user-emacs-directory "org-clock-save")
   org-id-locations-file (join-paths user-emacs-directory "org-id-locations")
   org-columns-default-format "%TODO %30ITEM %4PRIORITY %CLOCKSUM %EFFORT %CREATED %TAGS"))

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
  :hook (org-agenda-mode . hl-line-mode)
  :bind 
  ("C-c a" . org-agenda)
  ("C-c A" . org-agenda-show-week-all)
  ("C-c v" . org-tags-view)
  :init
  (defun nerd-agenda-icons (fun prefix alist)
    "Makes an org agenda alist"
    (mapcar (pcase-lambda (`(,category . ,icon))
              `(,category
                (,(funcall fun (concat prefix icon) :height 1.0))))
            alist))
  (mapadd org-agenda-category-icon-alist
          (nerd-agenda-icons #'nerd-icons-mdicon "nf-md-"
                             '(("alien" . "alien")
                               ("lib" . "library")
                               ("life" . "walk")
                               ("work" . "briefcase")
                               ("inbox" . "inbox")
                               ("archive" . "archive")
                               ("rnd" . "ufo")
                               ("std" . "toolbox")
                               ("graph" . "graph")
                               ("project" . "floor_plan")
                               ("roadmap" . "map")
                               ("shed" . "warehouse"))))
  (mapadd org-agenda-category-icon-alist
          (nerd-agenda-icons #'nerd-icons-sucicon "nf-custom-"
                             '(("emacs" . "emacs")
                               ("org" . "orgmode")
                               ("core" . "common_lisp"))))
  (setq org-agenda-include-diary t
        org-agenda-include-inactive-timestamps t
        org-agenda-span 7
        org-agenda-block-separator ?-
        org-agenda-breadcrumbs-separator (nerd-icons-mdicon "nf-md-menu_right")
        org-agenda-start-with-log-mode t
        org-agenda-columns-add-appointments-to-effort-sum t)
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
  :after (org)
  :hook (kill-emacs . org-id-locations-save)
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
  :load-path site-lisp-directory
  :after (org org-id)
  :hook (org-after-todo-state-change . (lambda () (org-expire-insert-created) (org-id-get-create))))

(use-package org-web-tools
  :ensure t
  :after (org)
  :bind (:map org-mode-map
              ("C-c c l" . org-web-tools-insert-link-for-url)))

(use-package htmlize
  :ensure t
  :after (org))

(use-package ol-notmuch
  :ensure t
  :after (org))

;; (use-package ox-man
;;   :commands (org-man-export-to-man org-man-export-to-pdf))

;; (use-package auctex)

;;; Hexl
(use-package hexl
  :init (setq hexl-bits 8))

;;; Etags
(use-package etags-regen
  :init (setq etags-regen-create-on-completion t
              ;; etags-regen-tags-file
              )
  :config
  (add-to-list 'etags-regen-file-extensions "gen"))

;;; Desktop
(use-package desktop
  :defer nil
  :hook (emacs-startup . (lambda () (desktop-save-mode (if buffer-file-name -1 1))))
  :config
  (setopt desktop-auto-save-timeout 60
          desktop-base-file-name ".desktop"
          desktop-base-lock-name ".desktop.lock"
          desktop-save nil)
  (add-to-list 'desktop-path "."))

;;; Multisession
;; TODO 2026-06-05: 
(use-package multisession)

;;; Dictionary
(use-package dictionary
  :init (setq dictionary-server "dict.compiler.company"))

;;; Ispell
(use-package ispell
  :ensure-system-package (aspell hunspell)
  :init
  (setq ispell-program-name "hunspell"
        ispell-personal-dictionary (join-paths user-home-directory ".config/dictionary"))
  :hook 
  (mail-send . ispell-message))

(use-package flyspell
  :hook
  (org-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  (text-mode . flyspell-mode)
  (emacs-lisp-mode . flyspell-prog-mode))

;;; Core Extensions
(use-package scratch 
  :bind 
  ("C-c z" . scratch-buffer)
  ("C-c C-z" . scratch-new)
  ("C-c Z" . default-scratch-buffer)
  :defer nil
  :load-path site-lisp-directory)

(use-package organ 
  :defer nil
  :load-path site-lisp-directory
  :commands (org-list-files)
  :hook (org-after-todo-state-change . org-clock-in-wip))

(use-package graph 
  :load-path site-lisp-directory
  :hook (org-mode . org-graph-maybe-enable))

(use-package inbox
  :defer nil
  :load-path site-lisp-directory
  :bind ("C-c 1" . org-inbox-open)
  :after (org-expire)
  :config (org-inbox-init))

(use-package gen 
  :defer nil
  :load-path site-lisp-directory
  :hook (lisp-mode . gen-maybe-enable))

(use-package plan 
  :defer nil
  :load-path site-lisp-directory
  ;; used in org/meta/babel.org, called via org-dblocks in project
  ;; readmes.
  :ensure-system-package tokei)

(use-package skel 
  :load-path site-lisp-directory
  :defer nil
  :bind (:map project-prefix-map 
              ("RET" . project-skel-shell)
              ("a" . project-agenda)
              ("t" . project-todo-list)
              ("C" . project-capture)
              ("R" . project-load-registers)
              ("S" . project-save-registers))
  :interpreter "skel"
  :hook 
  (project-find-functions . project-try-skel)
  ;; (hack-dir-local-get-variables-functions . skel-dir-local--get-variables)
  (prog-mode . skel-minor-mode)
  (org-mode . skel-minor-mode)
  (conf-mode . skel-minor-mode)
  (dired-mode . skel-minor-mode)
  (after-init . skel-init))

(use-package mpk 
  :load-path site-lisp-directory)

(provide 'default)
;; default.el ends here
