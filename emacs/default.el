;;; default.el --- default config -*- lexical-binding: t -*-
;;; Code:
;;; Settings
(require 'util)
(put 'upcase-region 'disabled nil)
(put 'list-threads 'disabled nil)
(put 'list-timers 'disabled nil)
(setq show-paren-context-when-offscreen 'overlay)
(setq
 org-safe-remote-resources '("\\`https://cdn\\.compiler\\.company/org/clean\\.theme\\'")
 ;; tabs = bad (unless in makefile..)
 indent-tabs-mode nil
 make-backup-files nil
 auto-save-list-file-prefix (expand-file-name "auto-save/." user-emacs-directory)
 tramp-auto-save-directory (expand-file-name "auto-save/tramp/" user-emacs-directory)
 dired-free-space nil
 mml-attach-file-at-the-end t
 dired-mouse-drag-files t
 confirm-kill-emacs nil
 confirm-kill-processes nil
 use-short-answers t
 display-time-format "%Y-%m-%d %H:%M"
 ring-bell-function 'ignore
 completion-ignore-case t
 kill-region-dwim nil
 ;; NOTE 2023-11-04: you need to add the following lines to ~/.gnupg/gpg-agent.conf:
 ;; allow-emacs-pinentry
 ;; allow-loopback-pinentry
 epg-pinentry-mode 'loopback
 shr-use-colors nil
 shr-use-fonts nil
 shr-max-image-proportion 0.6
 shr-image-animate nil
 shr-discard-aria-hidden t
 bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory)
 set-mark-command-repeat-pop t
 tempo-interactive t
 emms-directory (expand-file-name "emms" user-emacs-directory)
 gnus-cache-directory (expand-file-name "gnus" user-emacs-directory)
 url-cache-directory (expand-file-name "url" user-emacs-directory)
 tab-always-indent 'complete
 shr-cookie-policy nil
 ;; NOTE 2023-11-04: EXPERIMENTAL
 ediff-floating-control-frame t
 register-use-preview nil
 shr-use-xwidgets-for-media t
 which-key-mode t
 view-read-only t)

(cl-pushnew (cons "melpa" "https://melpa.org/packages/") package-archives :test 'cl-equalp)

(add-packages
 ;; eglot-x ;; LSP extensions
 org-web-tools ;; web parsing
 ol-notmuch ;; mail links
 htmlize ;; html export
 citeproc
 cape
 consult
 embark-consult
 embark
 ;; all-the-icons all-the-icons-dired all-the-icons-ibuffer ;; icons
 nerd-icons nerd-icons-dired nerd-icons-corfu nerd-icons-completion
 nerd-icons-ibuffer
 hide-mode-line) ;; ui
;; bbdb
(package-refresh-contents)
(package-install-selected-packages t)

;;; Treesitter

;;(add-to-list 'treesit-extra-load-path "/usr/local/lib/")

;; (let ((grammar-dir "/usr/local/share/tree-sitter/"))
;;   (when (file-exists-p grammar-dir)
;;     (setq treesit-extra-load-path
;;           (append
;;            (flatten
;;             (mapcar
;;              (lambda (f)
;;                (unless (or (string= "." f) (string= ".." f))
;;                  (concat grammar-dir f)))
;;              (directory-files "/usr/local/share/tree-sitter")))
;;            treesit-extra-load-path))))

;;; Variables
(defvar user-emacs-lib-directory (expand-file-name (join-paths user-emacs-directory "lib")))
(defvar user-custom-file (expand-file-name (format "%s.el" user-login-name) user-emacs-directory))
(defvar user-home-directory (expand-file-name "~"))
(defvar user-lab-directory (expand-file-name "lab" user-home-directory))
(defvar user-stash-directory (expand-file-name ".stash" user-home-directory))
(defvar user-store-directory (expand-file-name ".store" user-home-directory))
(defvar user-mail-directory (expand-file-name "mail" user-home-directory))
(defvar user-org-stash-directory (expand-file-name "org" user-stash-directory))
(defvar default-theme 'leuven-dark)
(defvar company-source-directory (join-paths user-home-directory "comp"))
(defvar company-org-directory (join-paths company-source-directory "org"))
(defvar company-babel-file (join-paths company-org-directory "meta/babel.org"))
(defvar company-bibliography (join-paths company-org-directory "graph/refs.bib"))
(defvar company-domain "compiler.company")
(defvar company-name "The Compiler Company, LLC")
(defvar company-vc-domain "vc.compiler.company")
(defvar company-vc-url (format "https://%s" company-vc-domain))
(defvar company-home "the.compiler.company")
(defvar company-cdn-url "https://cdn.compiler.company")
(defvar emacs-config-source (join-paths company-source-directory "core/emacs"))
(add-to-load-path user-emacs-lib-directory (join-paths user-stash-directory "lisp/slime"))

(with-eval-after-load 'default
  (require 'ulang)
  (ulang-init)
  (require 'scrum)
  (require 'inbox)
  (require 'graph)
  (require 'skel)
  (require 'c2))

;;; Env
(require 'exec-path-from-shell)
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
                                      "LISP_HOME"))

(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))
(add-to-list 'exec-path (expand-file-name "~/.local/bin/"))
(add-to-list 'exec-path "/bin/")
(add-to-list 'exec-path "/usr/local/sbin/")
(add-to-list 'exec-path "/usr/local/bin/")
(add-to-list 'exec-path "/usr/local/share/lisp/bin/")

;;; Completions
(use-package marginalia :ensure t
  :config (marginalia-mode))
;; avoid obsolete warnings about if-let -> if-let* etc
(use-package vertico
  :ensure t
  :config (vertico-mode)
  (keymap-set vertico-map "M-q" #'vertico-quick-insert)
  (keymap-set vertico-map "C-q" #'vertico-quick-exit))
;; (use-package kind-icon
;;   :ensure t
;;   :after corfu
;;                                         ;:custom
;;                                         ; (kind-icon-blend-background t)
;;                                         ; (kind-icon-default-face 'corfu-default) ; only needed with blend-background
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-overrides '((file (styles basic partial-completion))
                                        (eglot (styles orderless))
                                        (eglot-capf (styles orderless)))))

(use-package corfu
  :ensure t
  :config
  (global-corfu-mode)
  ;; (corfu-popupinfo-mode)
  ;; (corfu-echo-mode)
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
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev t)
  ;; (add-to-list 'completion-at-point-functions #'cape-file)
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred)))))
  (keymap-set corfu-map "M-m" #'corfu-move-to-minibuffer)
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer))

;;; Desktop
(setopt desktop-dirname (expand-file-name "sessions" user-emacs-directory))

;;; Multisession
(setq multisession-storage 'sqlite)

;;; Kill Ring
(kill-ring-deindent-mode)

;;; VC
;; use rhg, fallback to hg. see hgrc
(if (file-exists-p "~/.local/bin/rhg")
    (setq hg-binary "~/.local/bin/rhg"))

;;; Dired
(setq dired-dwim-target t
      dired-free-space 'separate)

;;; Speedbar
(require 'speedbar)
(setq speedbar-sort-tags t
      speedbar-prefer-window t
      speedbar-track-mouse-flag t)

(add-hook 'speedbar-after-create-hook 'turn-on-hide-mode-line-mode)

;;; Projects
(setopt  project-list-file (expand-file-name "projects" user-emacs-directory)
         project-mode-line t
         project-file-history-behavior 'relativize)

(defun remember-project ()
  (interactive)
  (project-remember-project (project-current))
  project--list)

(defun remember-lab-projects ()
  (interactive)
  (project-remember-projects-under user-lab-directory t))

(defun remember-comp-projects ()
  (interactive)
  (project-remember-projects-under company-source-directory t))

;;; Tabs
(add-hook 'tab-bar-mode-hook #'tab-bar-history-mode)

;;; Lisp
(use-package company :ensure t)
(require 'slime "slime")
(defvar core-lisp-program "/usr/local/bin/core")
(defun default-lisp () 
  (if (file-exists-p core-lisp-program)
      core-lisp-program
    "sbcl"))

(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

(setq inferior-lisp-program (format "%s --dynamic-space-size=8G --control-stack-size=32"
                                    (default-lisp))
      scheme-program-name "gsi"
      slime-auto-start t
      guile-program "guile"
      cmulisp-program "lisp"
      scsh-program "scsh")
(require 'slime-company "slime-company")
(require 'slime-cape "slime-cape")
(require 'slime-repl-ansi-color "slime-repl-ansi-color")
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
                       slime-repl-ansi-color
                       ;; slime-highlight-edits
                       slime-asdf))
(put 'make-instance 'common-lisp-indent-function 1)
(put 'reinitialize-instance 'common-lisp-indent-function 1)
(slime-setup slime-contribs)
;; X11-only (mcclim requires clx)
(defun clouseau-inspect (string)
  "Inspect a lisp value with Clouseau. make sure to load clouseau
with a custom core or in your init file before using this
function: '(ql:quickload :clouseau)'."
  (interactive
   (list (slime-read-from-minibuffer
          "Inspect value (evaluated): "
          (slime-sexp-at-point))))
  (let ((inspector 'cl-user::*clouseau-inspector*))
    (slime-eval-async
        `(cl:progn
          (cl:defvar ,inspector nil)
          ;; (Re)start the inspector if necessary.
          (cl:unless (cl:and (clim:application-frame-p ,inspector)
                             (clim-internals::frame-process ,inspector))
                     (cl:setf ,inspector (cl:nth-value 1 (clouseau:inspect nil :new-process t))))
          ;; Tell the inspector to visualize the correct datum.
          (cl:setf (clouseau:root-object ,inspector :run-hook-p t)
                   (cl:eval (cl:read-from-string ,string)))
          ;; Return nothing.
          (cl:values)))))

;; rebind the defpackage-regexp function to include DEFPKG
(setq slime-defpackage-regexp
      "^(\\(cl:\\|common-lisp:\\|uiop:\\|uiop/package:\\|std:\\|std/defpkg:\\|pkg:\\)?\\(defpackage\\|define-package\\|defpkg\\)\\>[ \t']*")

(define-common-lisp-style 
 "core" 
 "Core Common Lisp Indentation Style"
 (:inherit "sbcl")
 (:indentation
  (defpkg (as defpackage))
  (defpackage* (as defpackage))
  (blasfunc 2)
  (org-parse 2)
  (lety (as let))
  (lety* (as let*))
  (letv (as let))
  (letv* (as let*))
  (deferror (as define-condition))
  (plet (as let))
  (defwarning (as define-condition))
  (make-db (as make-instance))
  (make-palette (as defpackage))
  (define-package (as defpackage))
  (walk-directory 1)
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
      (buffer-string)))))

;; lisp font-lock defaults: https://www.n16f.net/blog/custom-font-lock-configuration-in-emacs/
;; (defface cl-character-face
;;   '((default :inherit font-lock-constant-face))
;;   "The face used to highlight Common Lisp character literals.")

;; (defface cl-standard-function-face
;;   '((default :inherit font-lock-keyword-face))
;;   "The face used to highlight standard Common Lisp function symbols.")

;; (defface cl-standard-value-face
;;   '((default :inherit font-lock-variable-name-face))
;;   "The face used to highlight standard Common Lisp value symbols.")

;; (defvar cl-font-lock-keywords
;;   (let* ((character-re (concat "#\\\\" lisp-mode-symbol-regexp "\\_>"))
;;          (function-re (concat "(" (regexp-opt cl-function-names t) "\\_>"))
;;          (value-re (regexp-opt cl-value-names 'symbols)))
;;     `((,character-re . 'cl-character-face)
;;       (,function-re
;;        (1 'cl-standard-function-face))
;;       (,value-re . 'cl-standard-value-face))))

(setq common-lisp-style-default "core")
;; (define-key slime-prefix-map (kbd "i") 'clouseau-inspect)
(setq slime-threads-update-interval 1)
(add-hook 'lisp-mode-hook 'slime-cape-enable)
(add-hook 'slime-repl-mode-hook 'slime-cape-enable)

;;; Eglot
;; (with-eval-after-load 'eglot
;;   (unless (package-installed-p 'eglot-x)
;;     (package-vc-install '(eglot-x :url "https://vc.compiler.company/packy/eglot-x")))
;;   (require 'eglot-x)
;;   (with-eval-after-load 'eglot-x
;;     (add-to-list 'eglot-server-programs
;;                  '((rust-ts-mode rust-mode) .
;;                    ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))
;;     (eglot-x-setup)))

;;; Asm
(require 'x86-lookup "x86-lookup")
(setq  x86-lookup-pdf "/opt/store/data/doc/64-iA32-isa.pdf")
(use-package nasm-mode :ensure t)
(add-hook 'asm-mode-hook 'nasm-mode)

;;; Rust
(add-hook 'rust-mode-hook 'eglot-ensure)

(setq rust-rustfmt-switches nil
      rust-indent-offset 2)

;;; Python
(setq python-indent-offset 2)
(add-hook 'python-mode-hook 'eglot-ensure)

;;; Javascript
(setq js-indent-level 2
      css-indent-offset 2)

;;; Bash
(setq sh-basic-offset 2)

;;; Graphviz
;; (use-package graphviz-dot-mode
;;   :ensure t
;;   :config
;;   (setq graphviz-dot-indent-width 2))

;;; Comments
(defcustom prog-comment-keywords
  '("TODO" "REVIEW" "FIX" "HACK" "RESEARCH")
  "List of strings with comment keywords."
  :group 'default
  :type '(list string))

(defcustom prog-comment-timestamp-format-concise "%F"
  "Specifier for date in `prog-comment-timestamp-keyword'.
Refer to the doc string of `format-time-string' for the available
options."
  :group 'default
  :type 'string)

(defcustom prog-comment-timestamp-format-verbose "%F %T %z"
  "Like `prog-comment-timestamp-format-concise', but longer."
  :group 'default
  :type 'string)

;;;###autoload
(defun prog-comment-dwim (arg)
  "Flexible, do-what-I-mean commenting.

If region is active and ARG is either a numeric argument greater
than one or a universal prefix (\\[universal-argument]), then
apply `comment-kill' on all comments in the region.

If the region is active and no ARG is supplied, or is equal to a
numeric prefix of 1, then toggle the comment status of the region.

Else toggle the comment status of the line at point.  With a
numeric prefix ARG, do so for ARGth lines (negative prefix
operates on the lines before point)."
  (interactive "p")
  (cond
   ((and (> arg 1) (use-region-p))
    (let* ((beg (region-beginning))
           (end (region-end))
           (num (count-lines beg end)))
      (save-excursion
        (goto-char beg)
        (comment-kill num))))
   ((use-region-p)
    (comment-or-uncomment-region (region-beginning) (region-end)))
   (t
    (save-excursion (comment-line (or arg 1))))))

(defvar prog-comment--keyword-hist '()
  "Input history of selected comment keywords.")

(defun prog-comment--keyword-prompt (keywords)
  "Prompt for candidate among KEYWORDS."
  (let ((def (car prog-comment--keyword-hist)))
    (completing-read
     (format "Select keyword [%s]: " def)
     keywords nil nil nil 'prog-comment--keyword-hist def)))


;;;###autoload
(defun prog-comment-timestamp-keyword (keyword &optional verbose)
  "Add timestamped comment with KEYWORD.

When called interactively, the list of possible keywords is that
of `prog-comment-keywords', though it is possible to
input arbitrary text.

If point is at the beginning of the line or if line is empty (no
characters at all or just indentation), the comment is started
there in accordance with `comment-style'.  Any existing text
after the point will be pushed to a new line and will not be
turned into a comment.

If point is anywhere else on the line, the comment is indented
with `comment-indent'.

The comment is always formatted as 'DELIMITER KEYWORD DATE:',
with the date format being controlled by the variable
`prog-comment-timestamp-format-concise'.

With optional VERBOSE argument (such as a prefix argument
`\\[universal-argument]'), use an alternative date format, as
specified by `prog-comment-timestamp-format-verbose'."
  (interactive
   (list
    (prog-comment--keyword-prompt prog-comment-keywords)
    current-prefix-arg))
  (let* ((date (if verbose
                   comment-timestamp-format-verbose
                 prog-comment-timestamp-format-concise))
         (string (format "%s %s: " keyword (format-time-string date)))
         (beg (point)))
    (cond
     ((or (eq beg (pos-bol))
          (default-line-regexp-p 'empty))
      (let* ((maybe-newline (unless (default-line-regexp-p 'empty 1) "\n")))
        ;; NOTE 2021-07-24: we use this `insert' instead of
        ;; `comment-region' because of a yet-to-be-determined bug that
        ;; traps `undo' to the two states between the insertion of the
        ;; string and its transformation into a comment.
        (insert
         (concat comment-start
                 ;; NOTE 2021-07-24: See function `comment-add' for
                 ;; why we need this.
                 (make-string
                  (comment-add nil)
                  (string-to-char comment-start))
                 comment-padding
                 string
                 comment-end))
        (indent-region beg (point))
        (when maybe-newline
          (save-excursion (insert maybe-newline)))))
     (t
      (comment-indent t)
      (insert (concat " " string))))))

(setq hexl-bits 8)
(setq tab-width 4)

;;; Keyboard Macros
(defun toggle-macro-recording ()
  (interactive)
  (if defining-kbd-macro
      (end-kbd-macro)
    (start-kbd-macro nil)))

(defun play-macro-if-not-playing ()
  (interactive)
  (if defining-kbd-macro
      (end-kbd-macro)
    (call-last-kbd-macro)))

;;; Registers
;; - additional register vtypes: buffer
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
  (set-register register (list 'file-query (buffer-file-name (current-buffer)) (point))))

;; additional register-val handlers
;; (cl-defmethod register-val-jump-to :around ((val cons) delete)
;;   (cond
;;    (t (cl-call-next-method val delete))))

;;; Outlines
(defun outline-hook (&optional rx)
  "Enable `outline-minor-mode' and set `outline-regexp'."
  (when rx (setq-local outline-regexp rx))
  (outline-minor-mode 1))

(setq outline-minor-mode-use-buttons nil)

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
               (skel-mode))

;;; Scratch
(defcustom default-scratch-buffer-mode 'lisp-interaction-mode
  "Default major mode for new scratch buffers"
  :group 'default
  :type 'symbol)

;; Adapted from the `scratch.el' package by Ian Eure.
(defun default-scratch-list-modes ()
  "List known major modes."
  (cl-loop for sym the symbols of obarray
           for name = (symbol-name sym)
           when (and (functionp sym)
                     (not (member sym minor-mode-list))
                     (string-match "-mode$" name)
                     (not (string-match "--" name)))
           collect name))

(defun default-scratch-buffer-setup (region &optional mode)
  "Add contents to `scratch' buffer and name it accordingly.

REGION is added to the contents to the new buffer.

Use the current buffer's major mode by default.  With optional
MODE use that major mode instead."
  (let* ((major (or mode major-mode))
         (string (format "Scratch buffer for: %s\n\n" major))
         (text (concat string region))
         (buf (format "*Scratch for %s*" major)))
    (with-current-buffer (get-buffer-create buf)
      (funcall major)
      (save-excursion
        (insert text)
        (goto-char (point-min))
        (comment-region (pos-bol) (pos-eol)))
      (vertical-motion 2))
    (pop-to-buffer buf)))

;;;###autoload
(defun default-scratch-buffer (&optional arg)
  "Produce a bespoke scratch buffer matching current major mode.

With optional ARG as a prefix argument (\\[universal-argument]),
use `default-scratch-buffer-mode'.

With ARG as a double prefix argument, prompt for a major mode
with completion.

If region is active, copy its contents to the new scratch
buffer."
  (interactive "P")
  (let* ((default-mode default-scratch-buffer-mode)
         (modes (default-scratch-list-modes))
         (region (with-current-buffer (current-buffer)
                   (if (region-active-p)
                       (buffer-substring-no-properties
                        (region-beginning)
                        (region-end))
                     "")))
         (m))
    (pcase (prefix-numeric-value arg)
      (16 (progn
            (setq m (intern (completing-read "Select major mode: " modes nil t)))
            (default-scratch-buffer-setup region m)))
      (4 (default-scratch-buffer-setup region default-mode))
      (_ (default-scratch-buffer-setup region)))))

;;;###autoload
(defun scratch-new ()
  "create a new scratch buffer. (could be *scratch* - *scratchN*)"
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname
                   (concat "*scratch"
                           (if (= n 0) "" (int-to-string n))
                           "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (insert initial-scratch-message)
    (lisp-interaction-mode)))

;;; Shell
(defun set-no-process-query-on-exit ()
  (let ((proc (get-buffer-process (current-buffer))))
    (when (processp proc)
      (set-process-query-on-exit-flag proc nil))))

(add-hook 'shell-mode-hook 'set-no-process-query-on-exit)
(add-hook 'term-exec-hook 'set-no-process-query-on-exit)

;;; Eshell
(defun eshell-new()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'Z))

(setq eshell-highlight-prompt t
      eshell-hist-ignoredups t
      eshell-save-history-on-exit t
      eshell-prefer-lisp-functions nil
      eshell-destroy-buffer-when-process-dies t)

(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell/alias "d" "dired $1")
            (eshell/alias "ff" "find-file $1")
            (eshell/alias "hgfe" "hg-fast-export.sh")))

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
                                (ring-elements eshell-history-ring)))))

;;; Eww
(setopt
 browse-url-browser-function 'eww
 eww-auto-rename-buffer 'title
 eww-search-prefix "https://google.com/search?q=")

(defun eww-at-point ()
  (interactive)
  (eww (thing-at-point 'url)))

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
    map))

(add-hook 'eww-mode-hook 'shr-heading-setup-imenu)
(add-hook 'eww-mode-hook (lambda () (define-key eww-mode-map "i" shr-heading-map)))

;;; ERC

;;; Tramp
(setopt tramp-default-method "ssh"
        tramp-default-user user-login-name
        tramp-default-host "localhost")

;;; Imenu
;; (use-package imenu-list :ensure t)

;;; Org
(require 'org)
(require 'org-agenda)
(require 'org-id)
(require 'org-protocol)

(use-package citeproc :ensure t)

(setq org-html-htmlize-output-type 'css
      org-html-head-include-default-style nil
      ;; cc default
      org-ascii-text-width 80
      org-attach-id-dir (join-paths company-cdn-url "media/"))

(org-crypt-use-before-save-magic)

(setq org-structure-template-alist
      '(("s" . "src")
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
        ("v" . "verse")))

(keymap-set org-mode-map "C-c l" 'org-follow-location)

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

(setq org-babel-default-header-args
      '((:session . "none") (:results . "replace") (:eval . "no-export") (:exports . "both")
	(:cache . "no") (:noweb . "no") (:hlines . "no") (:tangle . "no")))

;; org-sbx [[https://list.orgmode.org/d429d29b-42fa-7d7b-6f3a-9fe692fd6dc7@grinta.net/T/]]
(defun %org-sbx (name header args)
  (let* ((args (mapconcat
                (lambda (x)
                  (format "%s=%S" (symbol-name (car x)) (cadr x)))
                args ", "))
         (ctx (list 'babel-call (list :call name
                                      :name name
                                      :inside-header header
                                      :arguments args
                                      :end-header ":results silent")))
         (info (org-babel-lob-get-info ctx)))
    (when info (org-babel-execute-src-block nil info))))

(defmacro org-sbx (name &rest args)
  (let* ((header (if (stringp (car args)) (car args) nil))
	 (args (if (stringp (car args)) (cdr args) args)))
    (unless (stringp name)
      (setq name (symbol-name name)))
    (let ((result (%org-sbx name header args)))
      (org-trim (if (stringp result) result (format "%S" result))))))

(defun org-babel-execute-region (beg end &optional arg)
  (interactive "r")
  (narrow-to-region beg end)
  (org-babel-execute-buffer arg)
  (widen))

(defun org-schedule-effort ()
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let* ((element (org-element-at-point))
           (effort (org-element-property :EFFORT element))
           (scheduled (org-element-property :scheduled element))
           (ts-year-start (org-element-property :year-start scheduled))
           (ts-month-start (org-element-property :month-start scheduled))
           (ts-day-start (org-element-property :day-start scheduled))
           (ts-hour-start (org-element-property :hour-start scheduled))
           (ts-minute-start (org-element-property :minute-start scheduled)) )
      (org-schedule nil (concat
                         (format "%s" ts-year-start)
                         "-"
                         (if (< ts-month-start 10)
                             (concat "0" (format "%s" ts-month-start))
                           (format "%s" ts-month-start))
                         "-"
                         (if (< ts-day-start 10)
                             (concat "0" (format "%s" ts-day-start))
                           (format "%s" ts-day-start))
                         " "
                         (if (< ts-hour-start 10)
                             (concat "0" (format "%s" ts-hour-start))
                           (format "%s" ts-hour-start))
                         ":"
                         (if (< ts-minute-start 10)
                             (concat "0" (format "%s" ts-minute-start))
                           (format "%s" ts-minute-start))
                         "+"
                         effort)) )))

(setopt org-preview-latex-image-directory "~/.emacs.d/.cache/ltximg"
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
        org-clock-persist 'history)

(add-hook 'after-init-hook #'org-clock-persistence-insinuate)

;; archive
(defun extract-org-directory-titles-as-list (&optional dir)
  (interactive "D")
  (print
   (delete nil
           (let ((case-fold-search t))
             (mapcar (lambda (f)
                       (when (string-match "org$" f)
                         (with-temp-buffer
                           (insert-file-contents-literally
                            (concat (file-name-as-directory dir) f))
                           (while (and (not (looking-at-p "#\\+TITLE:"))
                                       (not (eobp)))
                             (forward-line))
                           (when (not (eobp))
                             (cons f (substring (thing-at-point 'line) 9 -1))))))
                     (directory-files dir))))))

(defun insert-directory-org-file-titles (&optional dir)
  (interactive "D")
  (let ((files-titles (extract-org-directory-titles-as-list dir)))
    (dolist (ft files-titles)
      (insert (concat "[[file:" (car ft)"][" (cdr ft) "]]\n")))))

(defun insert-directory-org-files (&optional dir)
  (interactive "D")
  (let ((files (directory-files dir)))
    (dolist (f files)
      (insert (concat "[[file:" f "][" (file-name-base f) "]]\n")))))

(defun include-directory-org-files (&optional dir)
  (interactive "D")
  (let ((files (directory-files dir)))
    (dolist (f files)
      (insert (concat "#+INCLUDE: " f "\n")))))

(defun org-todo-at-date (date)
  "create a todo entry for a given date."
  (interactive (list (org-time-string-to-time (org-read-date))))
  (cl-flet ((org-current-effective-time (&rest r) date)
            (org-today (&rest r) (time-to-days date)))
    (cond ((eq major-mode 'org-mode) (org-todo))
          ((eq major-mode 'org-agenda-mode) (org-agenda-todo)))))

(defun org-agenda-show-week-all (&optional arg ) (interactive "P") (org-agenda arg "n"))

(defun org-ask-location ()
  "prompt for a location."
  (let* ((org-refile-targets '((nil :maxlevel . 9)))
         (hd (condition-case nil
                 (car (org-refile-get-location))
               (error (car org-refile-history)))))
    (goto-char (point-min))
    (outline-next-heading)
    (if (re-search-forward
         (format org-complex-heading-regexp-format (regexp-quote hd))
         nil t)
        (goto-char (line-beginning-position))
      (goto-char (point-max))
      (or (bolp) (insert "\n"))
      (insert "* " hd "\n")))
  (end-of-line))

(defun org-capture-fileref-snippet (f type headers func-name)
  (let* ((code-snippet
          (buffer-substring-no-properties (mark) (- (point) 1)))
         (file-name   (buffer-file-name))
         (file-base   (file-name-nondirectory file-name))
         (line-number (line-number-at-pos (region-beginning)))
         (initial-txt (if (null func-name)
                          (format "From [[file:%s::%s][%s]]:"
                                  file-name line-number file-base)
                        (format "From ~%s~ (in [[file:%s::%s][%s]]):"
                                func-name file-name line-number
                                file-base))))
    (format "
    %s
    #+BEGIN_%s %s
 %s
    #+END_%s" initial-txt type headers code-snippet type)))

(defun org-capture-clip-snippet (f)
  "Given a file, F, this captures the currently selected text
     within an Org EXAMPLE block and a backlink to the file."
  (with-current-buffer (find-buffer-visiting f)
    (org-capture-fileref-snippet f "EXAMPLE" "" nil)))

(defun org-capture-code-snippet (f)
  "Given a file, F, this captures the currently selected text
     within an Org SRC block with a language based on the current mode
     and a backlink to the function and the file."
  (with-current-buffer (find-buffer-visiting f)
    (let ((org-src-mode (replace-regexp-in-string "-mode" "" (format "%s" major-mode)))
          (func-name (which-function)))
      (org-capture-fileref-snippet f "SRC" org-src-mode func-name))))

(defun region-to-clocked-task (start end)
  "Copies the selected text to the currently clocked in org-mode task."
  (interactive "r")
  (org-capture-string (buffer-substring-no-properties start end) "3"))

(setq org-global-properties
      '(quote (("EFFORT_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
               ("STYLE_ALL" . "habit"))))

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

;;;###autoload
(defun org-adjust-tags-column-reset-tags ()
  "In org-mode buffers it will reset tag position according to
`org-tags-column'."
  (when (and
         (not (string= (buffer-name) "*Remember*"))
         (eql major-mode 'org-mode))
    (let ((b-m-p (buffer-modified-p)))
      (condition-case nil
          (save-excursion
            (goto-char (point-min))
            (command-execute 'outline-next-visible-heading)
            ;; disable (message) that org-set-tags generates
            (cl-flet ((message (&rest ignored) nil))
              (org-set-tags 1 t))
            (set-buffer-modified-p b-m-p))
        (error nil)))))

;; TODO 2024-08-05: infer logbook column-titles/props
(defun column-display-value-transformer (column-title value)
  "Modifies the value to display in column view."
  (let ((title (upcase column-title)))
    (when (and (member title '("UPDATED" "NOTE")))
      (org-back-to-heading)
      (re-search-forward
       "Note taken on \\[\\(.*\\)\\] \\\\\\\\\\\n +\\(.*\\) *$"
       (org-entry-end-position) t))
    (if (equal column-title "UPDATED")
        (match-string-no-properties 1)
      (match-string-no-properties 2))))

(setq org-columns-modify-value-for-display-function
      #'column-display-value-transformer)

;;;###autoload
(defun org-align-all-tables ()
  "align all tables in current buffer"
  (interactive)
  (org-table-map-tables 'org-table-align 'quietly))

(defun org-remove-redundant-tags ()
  "Remove redundant tags of headlines in current buffer.

A tag is considered redundant if it is local to a headline and
inherited by a parent headline."
  (interactive)
  (when (eq major-mode 'org-mode)
    (save-excursion
      (org-map-entries
       (lambda ()
         (let ((alltags (split-string (or (org-entry-get (point) "ALLTAGS") "") ":"))
               local inherited tag)
           (dolist (tag alltags)
             (if (get-text-property 0 'inherited tag)
                 (push tag inherited) (push tag local)))
           (dolist (tag local)
             (if (member tag inherited) (org-toggle-tag tag 'off)))))
       t nil))))

;;;; Agenda
(cl-pushnew '("i" "Work in progress tasks" ((todo "WIP") (agenda))) org-agenda-custom-commands)

(defvar org-agenda-overriding-header)
(defvar org-agenda-sorting-strategy)
(defvar org-agenda-restrict)
(defvar org-agenda-restrict-begin)
(defvar org-agenda-restrict-end)

;;;###autoload
(defun org-agenda-reschedule-to-today ()
  (interactive)
  (cl-flet ((org-read-date (&rest rest) (current-time)))
    (call-interactively 'org-agenda-schedule)))

;; Patch org-mode to use vertical splitting
(defadvice org-prepare-agenda (after org-fix-split)
  (toggle-window-split))
(ad-activate 'org-prepare-agenda)

(add-hook 'org-agenda-mode-hook (lambda () (hl-line-mode 1)))

(defun org-agenda-log-mode-colorize-block ()
  "Set different line spacing based on clock time duration."
  (save-excursion
    (let* ((colors (cl-case (alist-get 'background-mode (frame-parameters))
                     (light
                      (list "#F6B1C3" "#FFFF9D" "#BEEB9F" "#ADD5F7"))
                     (dark
                      (list "#aa557f" "DarkGreen" "DarkSlateGray" "DarkSlateBlue"))))
           pos
           duration)
      (nconc colors colors)
      (goto-char (point-min))
      (while (setq pos (next-single-property-change (point) 'duration))
        (goto-char pos)
        (when (and (not (equal pos (pos-bol)))
                   (setq duration (org-get-at-bol 'duration)))
          ;; larger duration bar height
          (let ((line-height (if (< duration 15) 1.0 (+ 0.5 (/ duration 30))))
                (ov (make-overlay (pos-bol) (1+ (pos-eol)))))
            (overlay-put ov 'face `(:background ,(car colors) :foreground "black"))
            (setq colors (cdr colors))
            (overlay-put ov 'line-height line-height)
            (overlay-put ov 'line-spacing (1- line-height))))))))

(add-hook 'org-agenda-finalize-hook #'org-agenda-log-mode-colorize-block)

;;;###autoload
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
      (message nil))))

(defun org-export-translate-to-lang (term-translations &optional lang)
  "Adds desired translations to `org-export-dictionary'.
   TERM-TRANSLATIONS is alist consisted of term you want to translate
   and its corresponding translation, first as :default then as :html and
   :utf-8. LANG is language you want to translate to."
  (dolist (term-translation term-translations)
    (let* ((term (car term-translation))
           (translation-default (nth 1 term-translation))
           (translation-html (nth 2 term-translation))
           (translation-utf-8 (nth 3 term-translation))
           (term-list (assoc term org-export-dictionary))
           (term-langs (cdr term-list)))
      (setcdr term-list (append term-langs
                                (list
                                 (list lang
                                       :default translation-default
                                       :html translation-html
                                       :utf-8 translation-utf-8)))))))

(defun org-word-count (beg end
			   &optional count-latex-macro-args?
			   count-footnotes?)
  "Report the number of words in the Org mode buffer or selected region.
Ignores:
- comments
- tables
- source code blocks (#+BEGIN_SRC ... #+END_SRC, and inline blocks)
- hyperlinks (but does count words in hyperlink descriptions)
- tags, priorities, and TODO keywords in headers
- sections tagged as 'not for export'.

The text of footnote definitions is ignored, unless the optional argument
COUNT-FOOTNOTES? is non-nil.

If the optional argument COUNT-LATEX-MACRO-ARGS? is non-nil, the word count
includes LaTeX macro arguments (the material between {curly braces}).
Otherwise, and by default, every LaTeX macro counts as 1 word regardless
of its arguments."
  (interactive "r")
  (unless mark-active
    (setf beg (point-min)
	  end (point-max)))
  (let ((wc 0)
	(latex-macro-regexp "\\\\[A-Za-z]+\\(\\[[^]]*\\]\\|\\){\\([^}]*\\)}"))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
	(cond
	 ;; Ignore comments.
	 ((or (org-at-comment-p) (org-at-table-p))
	  nil)
	 ;; Ignore hyperlinks. But if link has a description, count
	 ;; the words within the description.
	 ((looking-at org-bracket-link-analytic-regexp)
	  (when (match-string-no-properties 5)
	    (let ((desc (match-string-no-properties 5)))
	      (save-match-data
		(cl-incf wc (length (remove "" (org-split-string
						desc "\\W")))))))
	  (goto-char (match-end 0)))
	 ((looking-at org-any-link-re)
	  (goto-char (match-end 0)))
	 ;; Ignore source code blocks.
	 ((org-between-regexps-p "^#\\+BEGIN_SRC\\W" "^#\\+END_SRC\\W")
	  nil)
	 ;; Ignore inline source blocks, counting them as 1 word.
	 ((save-excursion
	    (backward-char)
	    (looking-at org-babel-inline-src-block-regexp))
	  (goto-char (match-end 0))
	  (setf wc (+ 2 wc)))
	 ;; Count latex macros as 1 word, ignoring their arguments.
	 ((save-excursion
	    (backward-char)
	    (looking-at latex-macro-regexp))
	  (goto-char (if count-latex-macro-args?
			 (match-beginning 2)
		       (match-end 0)))
	  (setf wc (+ 2 wc)))
	 ;; Ignore footnotes.
	 ((and (not count-footnotes?)
	       (or (org-footnote-at-definition-p)
		   (org-footnote-at-reference-p)))
	  nil)
	 (t
	  (let ((contexts (org-context)))
	    (cond
	     ;; Ignore tags and TODO keywords, etc.
	     ((or (assoc :todo-keyword contexts)
		  (assoc :priority contexts)
		  (assoc :keyword contexts)
		  (assoc :checkbox contexts))
	      nil)
	     ;; Ignore sections marked with tags that are
	     ;; excluded from export.
	     ((assoc :tags contexts)
	      (if (intersection (org-get-tags-at) org-export-exclude-tags
				:test 'equal)
		  (org-forward-same-level 1)
		nil))
	     (t
	      (cl-incf wc))))))
	(re-search-forward "\\w+\\W*")))
    (format "%d words in %s." wc
	    (if mark-active "region" "buffer"))))

(defun org-check-misformatted-subtree ()
  "Check misformatted entries in the current buffer."
  (interactive)
  (show-all)
  (org-map-entries
   (lambda ()
     (when (and (move-beginning-of-line 2)
		(not (looking-at org-heading-regexp)))
       (if (or (and (org-get-scheduled-time (point))
		    (not (looking-at (concat "^.*" org-scheduled-regexp))))
	       (and (org-get-deadline-time (point))
		    (not (looking-at (concat "^.*" org-deadline-regexp)))))
	   (when (y-or-n-p "Fix this subtree? ")
	     (message "Call the function again when you're done fixing this subtree.")
	     (recursive-edit))
	 (message "All subtrees checked."))))))

(defun org-sort-list-by-checkbox-type ()
  "Sort list items according to Checkbox state."
  (interactive)
  (org-sort-list
   nil ?f
   (lambda ()
     (if (looking-at org-list-full-item-re)
	 (cdr (assoc (match-string 3)
		     '(("[X]" . 1) ("[-]" . 2) ("[ ]" . 3) (nil . 4))))
       4))))

(defun org-time-string-to-seconds (s)
  "Convert a string HH:MM:SS to a number of seconds."
  (cond
   ((and (stringp s)
	 (string-match "\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)" s))
    (let ((hour (string-to-number (match-string 1 s)))
	  (min (string-to-number (match-string 2 s)))
	  (sec (string-to-number (match-string 3 s))))
      (+ (* hour 3600) (* min 60) sec)))
   ((and (stringp s)
	 (string-match "\\([0-9]+\\):\\([0-9]+\\)" s))
    (let ((min (string-to-number (match-string 1 s)))
	  (sec (string-to-number (match-string 2 s))))
      (+ (* min 60) sec)))
   ((stringp s) (string-to-number s))
   (t s)))

(defun org-time-seconds-to-string (secs)
  "Convert a number of seconds to a time string."
  (cond ((>= secs 3600) (format-seconds "%h:%.2m:%.2s" secs))
	((>= secs 60) (format-seconds "%m:%.2s" secs))
	(t (format-seconds "%s" secs))))

(defmacro with-time (time-output-p &rest exprs)
  "Evaluate an org-table formula, converting all fields that look
like time data to integer seconds.  If TIME-OUTPUT-P then return
the result as a time value."
  (list
   (if time-output-p 'org-time-seconds-to-string 'identity)
   (cons 'progn
	 (mapcar
	  (lambda (expr)
	    `,(cons (car expr)
		    (mapcar
		     (lambda (el)
		       (if (listp el)
			   (list 'with-time nil el)
			 (org-time-string-to-seconds el)))
		     (cdr expr))))
	  `,@exprs))))

(defun org-hex-strip-lead (str)
  (if (and (> (length str) 2) (string= (substring str 0 2) "0x"))
      (substring str 2) str))

(defun org-hex-to-hex (int)
  (format "0x%x" int))

(defun org-hex-to-dec (str)
  (cond
   ((and (stringp str)
	 (string-match "\\([0-9a-f]+\\)" (setf str (org-hex-strip-lead str))))
    (let ((out 0))
      (mapc
       (lambda (ch)
	 (setf out (+ (* out 16)
		      (if (and (>= ch 48) (<= ch 57)) (- ch 48) (- ch 87)))))
       (coerce (match-string 1 str) 'list))
      out))
   ((stringp str) (string-to-number str))
   (t str)))

(defmacro with-hex (hex-output-p &rest exprs)
  "Evaluate an org-table formula, converting all fields that look
    like hexadecimal to decimal integers.  If HEX-OUTPUT-P then
    return the result as a hex value."
  (list
   (if hex-output-p 'org-hex-to-hex 'identity)
   (cons 'progn
	 (mapcar
	  (lambda (expr)
	    `,(cons (car expr)
		    (mapcar (lambda (el)
			      (if (listp el)
				  (list 'with-hex nil el)
				(org-hex-to-dec el)))
			    (cdr expr))))
	  `,@exprs))))

(require 'mm-url) ; to include mm-url-decode-entities-string

(cl-defun get-first-url (&optional (match (rx bol "http" (optional "s") "://")))
  "Return URL in clipboard, or first URL in the `kill-ring' matching MATCH."
  (cl-loop for item in (cons (current-kill 0) kill-ring)
	   when (and item (string-match-p match item))
	   return item))

(defun get-html-title-from-url (url)
  "Return content in <title> tag."
  (interactive (list (get-first-url)))
  (let (x1 x2 (download-buffer (url-retrieve-synchronously url)))
    (save-excursion
      (set-buffer download-buffer)
      (beginning-of-buffer)
      (setq x1 (search-forward "<title>"))
      (search-forward "</title>")
      (setq x2 (search-backward "<"))
      (mm-url-decode-entities-string (buffer-substring-no-properties x1 x2)))))

(defun org-insert-link-with-title (url)
  "Insert org link where default description is set to html title."
  (interactive (list (get-first-url match)))
  (let ((title (get-html-title-from-url url)))
    (org-insert-link nil url title)))

(defun org-insert-so-link (url)
  (interactive (list (get-first-url (rx bol "https://" (* anychar) "stackoverflow.com"))))
  (let ((title (get-html-title-from-url url)))
    (org-insert-link nil url title)))

(defun org-remove-empty-propert-drawers ()
  "*Remove all empty property drawers in current file."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "You need to turn on Org mode for this function."))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ":PROPERTIES:" nil t)
      (save-excursion
	(org-remove-empty-drawer-at "PROPERTIES" (match-beginning 0))))))

(defun check-for-clock-out-note ()
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (let ((tags (org-get-tags)))
      (and tags (message "tags: %s " tags)
	   (when (member "clocknote" tags)
	     (org-add-note))))))

(add-hook 'org-clock-out-hook 'check-for-clock-out-note)

(defun org-list-files (dirs ext)
  "Function to create list of org files in multiple subdirectories.
This can be called to generate a list of files for
org-agenda-files or org-refile-targets.

DIRS is a list of directories.

EXT is a list of the extensions of files to be included."
  (let ((dirs (if (listp dirs)
		  dirs
		(list dirs)))
	(ext (if (listp ext)
		 ext
	       (list ext)))
	files)
    (mapc
     (lambda (x)
       (mapc
	(lambda (y)
	  (setq files
		(append files
			(file-expand-wildcards
			 (concat (file-name-as-directory x) "*" y)))))
	ext))
     dirs)
    (mapc
     (lambda (x)
       (when (or (string-match "/.#" x)
		 (string-match "#$" x))
	 (setq files (delete x files))))
     files)
    files))

;;; Dictionary
(setq dictionary-server "compiler.company"
      switch-to-buffer-obey-display-actions t)

;;; Ispell
;; requires aspell and a hunspell dictionary (hunspell-en_us)
(setq-default ispell-program-name "hunspell")
(add-hook 'mail-send-hook  #'ispell-message)

;;; Skel
(require 'skel)
(require 'skt)

(provide 'default)
;; default.el ends here
