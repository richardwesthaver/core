;;; default.el --- default config -*- lexical-binding: t -*-

;;; Code:

;;; Settings
(put 'upcase-region 'disabled nil)
(put 'list-threads 'disabled nil)
(put 'list-timers 'disabled nil)

(setq-default 
 make-backup-files nil
 auto-save-list-file-prefix (expand-file-name "auto-save/." user-emacs-directory)
 tramp-auto-save-directory (expand-file-name "auto-save/tramp/" user-emacs-directory)
 dired-free-space nil
 confirm-kill-emacs nil
 confirm-kill-processes nil
 use-short-answers t
 display-time-format "%Y-%m-%d %H:%M"
 ring-bell-function 'ignore
 completion-ignore-case t
 epg-pinentry-mode 'loopback
 shr-use-colors nil
 shr-use-fonts nil
 shr-max-image-proportion 0.6
 shr-image-animate nil
 shr-discard-aria-hidden t
 bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory)
 project-list-file (expand-file-name "projects" user-emacs-directory)
 emms-directory (expand-file-name "emms" user-emacs-directory)
 gnus-cache-directory (expand-file-name "gnus" user-emacs-directory)
 url-cache-directory (expand-file-name "url" user-emacs-directory)
 tab-always-indent 'complete
 shr-cookie-policy nil
 browse-url-browser-function 'browse-url-default-browser
 eww-search-prefix "https://duckduckgo.com/html?q="
 url-privacy-level '(email agent cookies lastloc)
 view-read-only t
 tramp-default-method "sshx")

(when-sys= "darwin"
  (setq-default dired-use-ls-dired nil))

;;; Variables
(defvar default-theme 'modus-vivendi-deuteranopia)

(defvar company-domain "compiler.company")
(defvar company-name "The Compiler Company, LLC")
(defvar company-vc-domain "vc.compiler.company")
(defvar company-home "the.compiler.company")

(defvar user-home-directory (expand-file-name "~"))
(defvar user-lab-directory (expand-file-name "lab" user-home-directory))
(defvar user-stash-directory (expand-file-name "stash" user-home-directory))
(defvar user-store-directory (expand-file-name "store" user-home-directory))
(defvar user-shed-directory (expand-file-name "shed" user-home-directory))
(defvar user-mail-directory (expand-file-name "mail" user-home-directory))
(defvar user-media-directory (expand-file-name "media" user-home-directory))

;;; Theme
(add-hook 'after-init-hook (lambda () (load-theme default-theme)))

;;; Packages
(package-initialize)

(with-eval-after-load 'package
  (setq package-archives 
	'(("gnu" . "https://elpa.gnu.org/packages/")
	  ("nongnu" . "https://elpa.nongnu.org/nongnu/")
	  ("melpa" . "https://melpa.org/packages/"))
	use-package-always-ensure t
	use-package-expand-minimally t)
  (add-packages
   org-web-tools 
   citeproc 
   all-the-icons all-the-icons-dired all-the-icons-ibuffer
   slime
   rust-mode
   which-key
   tree-sitter-langs)
  (package-install-selected-packages t))

;;; Desktop
(add-hook 'kill-emacs-hook #'desktop-save)
;;; Multisession
(setq multisession-storage 'sqlite)

;;; VC
;; use rhg, fallback to hg. see hgrc
(if (file-exists-p "~/.local/bin/rhg")
        (setq hg-binary "~/.local/bin/rhg"))

;;; Dired
(add-hook 'dired-mode-hook #'all-the-icons-dired-mode)
(add-hook 'ibuffer-mode-hook #'all-the-icons-ibuffer-mode)

;;; Treesitter
(use-package tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;;; Lisp
(use-package lisp-mode
  :ensure nil
  :config
  (setq inferior-lisp-program "sbcl"
	scheme-program-name "gsi"
	guile-program "guile"
	cmulisp-program "lisp"
	scsh-program "scsh"))

(use-package slime
  :config
  (defvar slime-toggle nil)
  (defun slime-toggle ()
    "toggle between lisp file and slime-repl"
    (interactive)
    (if (eq major-mode 'slime-repl-mode)
        (setq slime-toggle (pop-to-buffer (or slime-toggle (read-buffer "lisp file: "))))
      (progn
        (setq slime-toggle (current-buffer))
        (slime-repl)))))

;; paredit-map
;; (defvar paredit-map
;;     (let ((map (make-sparse-keymap)))
;;     (pcase-dolist (`(,k . ,f)
;;                    '(("u" . backward-up-list)
;;                      ("f" . forward-sexp)
;;                      ("b" . backward-sexp)
;;                      ("d" . down-list)
;;                      ("k" . kill-sexp)
;;                      ("n" . paredit-forward)
;;                      ("p" . paredit-backward)
;;                      ("K" . paredit-kill)
;;                      ("]" . paredit-forward-slurp-sexp)
;;                      ("[" . paredit-backward-slurp-sexp)
;;                      ("}" . paredit-forward-barf-sexp)
;;                      ("{" . paredit-backward-barf-sexp)
;;                      ("C" . paredit-convolute-sexp)
;;                      ("J" . paredit-join-sexps)
;;                      ("S" . paredit-split-sexp)
;;                      ("R" . paredit-raise-sexp)
;;                      ("\\" . indent-region)
;;                      ("/" . undo)
;;                      ("t" . transpose-sexps)
;;                      ("x" . eval-defun)))
;;       (define-key map (kbd k) f))
;;     map))

;; (map-keymap
;;  (lambda (_ cmd)
;;      (put cmd 'repeat-map 'paredit-map))
;;  paredit-map)

;;; Rust
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))
(add-hook 'rust-mode-hook 'eglot-ensure)
(setq rust-rustfmt-switches nil
      rust-indent-offset 2)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer"))))

;;; Python
(setq python-indent-offset 2)
(add-hook 'python-mode-hook 'eglot-ensure)

;;; Bash
(setq sh-basic-offset 2)

;;; Comments
(defcustom prog-comment-keywords
  '("TODO" "REVIEW" "FIX" "HACK" "RESEARCH")
  "List of strings with comment keywords."
  :group 'default)

(defcustom prog-comment-timestamp-format-concise "%F"
  "Specifier for date in `prog-comment-timestamp-keyword'.
Refer to the doc string of `format-time-string' for the available
options."
  :group 'default)

(defcustom prog-comment-timestamp-format-verbose "%F %T %z"
  "Like `prog-comment-timestamp-format-concise', but longer."
  :group 'default)

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
     ((or (eq beg (point-at-bol))
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
(defun outline-hook (rx)
  "Enable `outline-minor-mode' and set `outline-regexp'."
  (setq-local outline-regexp rx)
  (outline-minor-mode t))

(defun add-outline-hook (mode rx)
    (let ((sym (symb mode "-hook")))
      (add-hook sym (lambda () (outline-hook rx)))))

(defmacro outline-hooks (&rest pairs)
  `(mapc (lambda (x) (add-outline-hook (car x) (cadr x))) ',pairs))

(outline-hooks (asm-mode ";;;+")
	       (nasm-mode ";;;+")	       
	       (rust-mode "\\(//!\\|////+\\)")
	       (sh-mode "###+")
	       (sh-script-mode "###+")
	       (makefile-mode "###+"))

;;; Scratch
(defcustom default-scratch-buffer-mode 'lisp-interaction-mode
    "Default major mode for new scratch buffers"
    :group 'default)

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
        (comment-region (point-at-bol) (point-at-eol)))
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
	    (eshell/alias "hgfe" "~/bin/sh/hg-fast-export.sh")))

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
;;; Org
;; todos
(setq org-todo-keywords
      '((type "TBD(0!)" "TODO(t!)" "|")
	(sequence "FIND(q!)" "READ(R@!)" "WATCH(W@!)" "|")
	(sequence "RESEARCH(s!)" "RECORD(e!)" "|")
	(sequence "OUTLINE(o!)" "RESEARCH(r!)" "DRAFT(m!)" "REVIEW(w!)" "|")
	(sequence "FIX(i!)" "TEST(t!)" "|")
	(type "GOTO(g!)" "HACK(h!)" "NOTE(n!)" "CODE(c!)" "LINK(l!)" "|")
	(type "KLUDGE(k@!)" "|")
	(sequence "|" "DONE(d!)" "NOPE(x@!)" "FOUND(f@!)")))
;; captures
(setq org-capture-templates
      '(("t" "task" entry (file "inbox.org") "* %^{title}\n- %?" :prepend t)
	("1" "current-task-item" item (clock) "%i%?")
	("2" "current-task-checkbox" checkitem (clock) "%i%?")
	("3" "current-task-region" plain (clock) "%i" :immediate-finish t :empty-lines 1)
	("4" "current-task-kill" plain (clock) "%c" :immediate-finish t :empty-lines 1)
	("l" "log" item (file+headline "log.org" "log") "%U %?" :prepend t)
	("s" "secret" table-line (file+function "krypt" org-ask-location) "| %^{key} | %^{val} |" :immediate-finish t :kill-buffer t)
	("n" "note" plain (file+function "notes.org" org-ask-location) "%?")
	("i" "idea" entry (file "inbox.org") "* OUTLINE %?\n:notes:\n:end:\n- _outline_ [/]\n  - [ ] \n  - [ ] \n- _refs_" :prepend t)
	("b" "bug" entry (file "inbox.org") "* FIX %?\n- _review_\n- _fix_\n- _test_" :prepend t)
	("r" "research" entry (file "inbox.org") "* RESEARCH %?\n:notes:\n:end:\n- _refs_" :prepend t)))
(setq org-html-htmlize-output-type 'css
      org-html-head-include-default-style nil)

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

(setq org-preview-latex-image-directory "~/.emacs.d/.cache/ltximg"
      org-latex-image-default-width "8cm")

(setq org-refile-use-cache t
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets '((nil :maxlevel . 3)
			   (org-agenda-files :maxlevel . 3)))

(setq org-confirm-babel-evaluate nil)

(setq org-src-fontify-natively t
      org-src-tabs-act-natively t)

(setq org-footnote-section nil)

(defun org-todo-at-date (date)
  "create a todo entry for a given date."
  (interactive (list (org-time-string-to-time (org-read-date))))
  (cl-flet ((org-current-effective-time (&rest r) date)
            (org-today (&rest r) (time-to-days date)))
    (cond ((eq major-mode 'org-mode) (org-todo))
          ((eq major-mode 'org-agenda-mode) (org-agenda-todo)))))

(defun org-ask-location ()
  "prompt for a location\"\""
  (let* ((org-refile-targets '((nil :maxlevel . 9)))
         (hd (condition-case nil
                 (car (org-refile-get-location))
               (error (car org-refile-history)))))
    (goto-char (point-min))
    (outline-next-heading)
    (if (re-search-forward
         (format org-complex-heading-regexp-format (regexp-quote hd))
         nil t)
        (goto-char (point-at-bol))
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
            (flet ((message (&rest ignored) nil))
		  (org-set-tags 1 t))
            (set-buffer-modified-p b-m-p))
        (error nil)))))

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
(defvar org-agenda-overriding-header)
(defvar org-agenda-sorting-strategy)
(defvar org-agenda-restrict)
(defvar org-agenda-restrict-begin)
(defvar org-agenda-restrict-end)

;;;###autoload
(defun org-agenda-reschedule-to-today ()
  (interactive)
  (flet ((org-read-date (&rest rest) (current-time)))
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
        (when (and (not (equal pos (point-at-eol)))
                   (setq duration (org-get-at-bol 'duration)))
          ;; larger duration bar height
          (let ((line-height (if (< duration 15) 1.0 (+ 0.5 (/ duration 30))))
                (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
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

;;; Tempo
(setq tempo-interactive t)

(provide 'default)
;; default.el ends here
