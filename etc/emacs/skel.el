;;; skel.el --- skel Emacs Mode -*- lexical-binding:t -*-

;; skel-mode, skel-minor-mode, skt-minor-mode

;; Copyright (C) 2023  The Compiler Company

;; Author: ellis <ellis@rwest.io>
;; Keywords: languages, lisp

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

;;

;;; Code:
(eval-and-compile 
  (require 'eieio)
  (require 'cl-lib)
  (require 'skeleton)
  (require 'project)
  (require 'org)
  (require 'tempo)
  (require 'autoinsert)
  (defvar skel-debug nil)
  (when skel-debug (require 'ede)))

;;; Custom
(defgroup skel nil
  "skel customization group."
  :group 'local)

(defcustom skel-map-prefix "C-x M-s"
  "Prefix for `skel' keymap."
  :type 'string
  :group 'skel)

(defcustom skel-shell-buffer-name "Skel"
  "Default buffer name for Skel interpreter."
  :type 'string
  :safe 'stringp
  :group 'skel)

(defcustom skel-shell-interpreter
  (cond ((executable-find "skel") "skel shell")
        (t "core"))
  "Skel interpreter for interactive use."
  :type 'string
  :group 'skel)

(defcustom skel-shell-interpreter-args ""
  "Arguments for the Skel interpreter for interactive use."
  :type 'string
  :group 'skel)

(defcustom skel-shell-dedicated nil
  "Whether to make Skel shells dedicated by default.
This option influences `run-skel' when called without a prefix
argument.  If `buffer' or `project', create a Skel shell
dedicated to the current buffer or its project (if one is found)."
  :version "29.1"
  :type '(choice (const :tag "To buffer" buffer)
                 (const :tag "To project" project)
                 (const :tag "Not dedicated" nil)))

;;; Commands
;; should dispatch to a server, likely covered by eglot tho..
(defvar-keymap skel-map
  :doc "skel keymap"
  :prefix 'skel-map
  "b" 'skel:build
  "m" 'skel:make
  "c" 'skel:compile
  "u" 'skel:update
  "U" 'skel:unpack
  "P" 'skel:pack
  "d" 'skel:dist
  "x" 'skel:clean
  "r" 'skel:run
  "s" 'skel:show
  "i" 'skel:install
  "v" 'skel:vc
  "V" 'skel:view)

(defmacro def-skel-cmd (name)
  `(defun ,(symb 'skel: name) (&optional arg)
     (interactive "P")
     (when arg (setf arg (read-string (format "skel %s " ',name))))
     (let ((default-directory (project-root (project-current t))))
       (async-shell-command (format "skel %s %s" ',name (princ (or arg "")))))))

(def-skel-cmd build)
(def-skel-cmd dist)
(def-skel-cmd compile)
(def-skel-cmd update)
(def-skel-cmd make)
(def-skel-cmd run)
(def-skel-cmd pack)
(def-skel-cmd install)
(def-skel-cmd unpack)
(def-skel-cmd show)
(def-skel-cmd vc)
(def-skel-cmd search)
(def-skel-cmd view)

(defun project-try-skel (dir)
  (when (or (file-exists-p (join-paths dir "skelfile"))
	        (directory-files dir nil "^.*[.]sk"))
    (let ((res (project-try-vc--search dir)))
      (when res 
	    (vc-file-setprop dir 'project-vc res)
	    (setf (car res) 'skel))
      (append res (list dir)))))

(defun skel-indent-region (start end)
  "Indent region as a SKEL S-expression."
  (save-excursion
    (setq end (copy-marker end))
    (goto-char start)
    (beginning-of-line)
    (let* ((parse-state (lisp-indent-initial-state))
	       (pr (unless (minibufferp)
		         (make-progress-reporter "Indenting region..." (point) end))))
      (let ((ppss (lisp-indent-state-ppss parse-state)))
	    (unless (or (and (bolp) (eolp)) (nth 3 ppss))
	      (lisp-indent-line (calculate-lisp-indent ppss))))
      (let ((indent nil))
	    (while (progn (setq indent (lisp-indent-calc-next parse-state))
		              (< (point) end))
	      (unless (or (and (bolp) (eolp)) (not indent))
	        (lisp-indent-line indent))
	      (and pr (progress-reporter-update pr (point)))))
      (and pr (progress-reporter-done pr))
      (move-marker end nil))))

(cl-defmethod project-root ((project (head skel)))
  (when (and project (>= (length project) 4))
    (caddr project)))

(cl-defmethod project-root ((project list))
  (when project (car project)))

(defun project-skelfile (&optional project)
  "Find skelfile associated with PROJECT. Defaults to current
directory and returns name of skelfile. When PROJECT is T uses
`project-current'."
  (interactive)
  (let* ((dir (or project (project-root (project-current)) default-directory))
         (project-root (project-root (project-current nil dir))))
    (or
     (when dir
       (cl-find-if 
        (lambda (x)
          (when (string-match
                 (rx (or "skelfile" (and (* any) ".sk")))
                 (file-name-nondirectory x))
            x))
        (directory-files dir t)))
     (when project
       (cl-find-if (lambda (x)
                     (when (string-match (rx (or "skelfile" (and (* any) ".sk")))
                                         (file-name-nondirectory x))
                       x))
                   (directory-files project-root t))))))

(defun read-skelfile-bind (&optional project)
  "Open PROJECT's skelfile and return the :bind form."
  (let ((buffer (find-file-noselect (project-skelfile project))))
    (with-current-buffer buffer
      (goto-char (point-min))
      (goto-char (search-forward-regexp (rx bol ":bind" (* space))))
      (read buffer))))

(defun project-skelfile-dir-locals (&optional project)
  "Return a list of dir-local bindings from a skelfile."
  (cl-block nil
    (dolist (f (read-skelfile-bind project))
      (when (eql (car f) :dir-locals) (cl-return (cdr f)))
      (when (eql (cadr f) :dir-locals)
        ;; when used as second element, the first is the name
        ;; of the CL-local binding, here we discard it and
        ;; just take the CDDR.
        (cl-return (cddr f))))))

(defun skel-dir-local--get-variables ()
  "Compute and return the list of :DIR-LOCAL bindings found in the current
project's skelfile, if any. Typically added to 'hack-dir-local-get-variables-functions'."
  (let ((root (project-root (project-current))))
    (when root
      (cons (expand-file-name root) (project-skelfile-dir-locals root)))))

(defun skel-dir-local-get-variables ()
  "Open the project skelfile and return the :dir-locals bindings if present."
  (let ((root (expand-file-name (project-root (project-current)))))
    (when root 
      (unless (assoc-string root dir-locals-class-alist t)
        (push (skel-dir-local--get-variables) dir-locals-class-alist)))))

;;; Shell
(defun clone-local-variables (from-buffer &optional regexp)
  "Clone local variables from FROM-BUFFER.
Optional argument REGEXP selects variables to clone and defaults
to \"^skel-\"."
  (mapc
   (lambda (pair)
     (and (consp pair)
          (symbolp (car pair))
          (string-match (or regexp "^skel-")
                        (symbol-name (car pair)))
          (set (make-local-variable (car pair))
               (cdr pair))))
   (buffer-local-variables from-buffer)))

(defvar skel-shell--parent-buffer nil)

(define-derived-mode inferior-skel-mode comint-mode "Inferior Skel"
  "Major mode for Skel inferior process.
Runs a Skel interpreter as a subprocess of Emacs, with Skel
I/O through an Emacs buffer.  Variables `skel-shell-interpreter'
and `skel-shell-interpreter-args' control how skel is run."
  (when skel-shell--parent-buffer (clone-local-variables skel-shell--parent-buffer))
  (setq-local indent-tabs-mode nil)
  (setq-local comint-output-filter-functions
              '(ansi-color-process-output
                ;; skel-shell-comint-watch-for-first-prompt-output-filter
                comint-watch-for-password-prompt))
  (setq-local scroll-conservatively 1)
  (setq-local comint-dynamic-complete-functions
              '(comint-c-a-p-replace-by-expanded-history))
  (compilation-shell-minor-mode 1))

(defun skel-shell-calculate-command ()
  "Calculate the string used to execute the inferior Skel process."
  (concat
   skel-shell-interpreter
   (unless (string-empty-p skel-shell-interpreter-args) " ")
   skel-shell-interpreter-args))

(defun skel-shell-make-comint (cmd proc-name &optional show internal)
  (save-excursion
    (let* ((proc-buffer-name
            (format (if (not internal) "*%s*" " *%s*") proc-name)))
      (when (not (comint-check-proc proc-buffer-name))
        (let* ((cmdlist (split-string-and-unquote cmd))
               (interpreter (car cmdlist))
               (args (cdr cmdlist))
               (buffer (apply #'make-comint-in-buffer proc-name
                              proc-buffer-name
                              interpreter nil args))
               (skel-shell--parent-buffer (current-buffer))
               (process (get-buffer-process buffer))
               (skel-shell-interpreter interpreter)
               (skel-shell-interpreter-args (mapconcat #'identity args " ")))
          (with-current-buffer buffer
            (inferior-skel-mode))
          (and internal (set-process-query-on-exit-flag process nil))))
      (when show (pop-to-buffer proc-buffer-name))
      proc-buffer-name)))

(defun skel-shell-get-process-name (dedicated)
  "Calculate the appropriate process name for inferior Skel process.
If DEDICATED is nil, this is simply `skel-shell-buffer-name'.
If DEDICATED is `buffer' or `project', append the current buffer
name respectively the current project name."
  (pcase dedicated
    ('nil skel-shell-buffer-name)
    ('project
     (if-let* ((proj (project-current)))
         (format "%s[%s]" skel-shell-buffer-name (project-name proj))
       skel-shell-buffer-name))
    (_ (format "%s[%s]" skel-shell-buffer-name (buffer-name)))))

(defun skel-shell-get-buffer ()
  "Return inferior Skel buffer for current buffer.
If current buffer is in `inferior-skel-mode', return it."
  (if (derived-mode-p 'inferior-skel-mode)
      (current-buffer)
    (seq-some
     (lambda (dedicated)
       (let* ((proc-name (skel-shell-get-process-name dedicated))
              (buffer-name (format "*%s*" proc-name)))
         (when (comint-check-proc buffer-name)
           buffer-name)))
     '(buffer project nil))))

(defun skel-shell-get-process ()
  "Return inferior Skel process for current buffer."
  (get-buffer-process (skel-shell-get-buffer)))

(defun skel-shell-get-process-or-error (&optional interactivep)
  "Return inferior Skel process for current buffer or signal error.
When argument INTERACTIVEP is non-nil, use `user-error' instead
of `error' with a user-friendly message."
  (or (skel-shell-get-process)
      (if interactivep
          (user-error
           (substitute-command-keys
            "Start a Skel process first with \\`M-x run-skel' or `%s'")
           ;; Get the binding.
           (key-description
            (or (where-is-internal #'run-skel overriding-local-map t)
                (where-is-internal #'project-skel-shell overriding-local-map t))))
        (error "No inferior Skel process running"))))

(defun skel-shell--save-temp-file (string)
  (let* ((temporary-file-directory
          (if (file-remote-p default-directory)
              (concat (file-remote-p default-directory) "/tmp")
            temporary-file-directory))
         (temp-file-name (make-temp-file "lisp"))
         ;; (coding-system-for-write (python-info-encoding))
         )
    (with-temp-file temp-file-name
      (if (bufferp string)
          (insert-buffer-substring string)
        (insert string))
      (delete-trailing-whitespace))
    temp-file-name))

(defun skel-shell-send-file (file-name &optional process delete temp-file-name msg)
  "Send FILE-NAME to inferior Skel PROCESS.

If TEMP-FILE-NAME is passed then that file is used for processing
instead, while internally the shell will continue to use FILE-NAME.
FILE-NAME can be remote, but TEMP-FILE-NAME must be in the same host as
PROCESS.  If TEMP-FILE-NAME and DELETE is non-nil, then TEMP-FILE-NAME is deleted
after evaluation is performed.

When optional argument MSG is non-nil, forces display of a
user-friendly message if there's no process running; defaults to
t when called interactively."
  (interactive
   (list
    (read-file-name "File to send: ")   ; file-name
    nil                                 ; process
    nil                                 ; delete
    nil                                 ; temp-file-name
    t))                                 ; msg
  (setq process (or process (skel-shell-get-process-or-error msg)))
  (with-current-buffer (process-buffer process)
    (unless (or temp-file-name
                (string= (file-remote-p file-name)
                         (file-remote-p default-directory)))
      (setq temp-file-name (with-temp-buffer
                             (insert-file-contents file-name)
                             (skel-shell--save-temp-file (current-buffer))))))
  (let* ((temp-file-name (when temp-file-name
                           (file-local-name (expand-file-name
                                             temp-file-name)))))
    (comint-send-string
     process
     (format
      "(load \"%s\")\n" temp-file-name))
    (delete-file temp-file-name)))

(defun skel-shell-send-string (string &optional process msg)
  "Send STRING to inferior Skel PROCESS.
When optional argument MSG is non-nil, forces display of a user-friendly
message if there's no process running; defaults to t when called
interactively."
  (interactive
   (list (read-string "Skel command: ") nil t))
  (let ((process (or process (skel-shell-get-process-or-error msg))))
    (unless skel-shell-output-filter-in-progress
      (with-current-buffer (process-buffer process)
        (save-excursion
          (goto-char (process-mark process))
          (insert-before-markers "\n"))))
    (if (null (process-tty-name process))
        (comint-send-string process string)
      (let* ((temp-file-name (with-current-buffer (process-buffer process)
                               (skel-shell--save-temp-file string)))
             (file-name (or (buffer-file-name) temp-file-name)))
        (skel-shell-send-file file-name process temp-file-name t)))))

(defun skel-shell-send-region (start end &optional msg)
  (interactive
   (list (region-beginning) (region-end) t))
  (let* ((string (buffer-substring-no-properties start end))
         (process (skel-shell-get-process-or-error msg))
         (_ (string-match "\\`\n*\\(.*\\)" string)))
    (message "Sent: %s..." (match-string 1 string))
    ;; Recalculate positions to avoid landing on the wrong line if
    ;; lines have been removed/added.
    (with-current-buffer (process-buffer process)
      (compilation-forget-errors))
    (skel-shell-send-string string process)
    (deactivate-mark)))

(defun skel-shell-send-buffer (&optional msg)
  (interactive (list t))
  (save-restriction
    (widen)
    (skel-shell-send-region (point-min) (point-max) msg)))

(defun run-skel (&optional cmd dedicated show)
  "Run an inferior Skel process."
  (interactive
   (if current-prefix-arg
       (list
        (read-shell-command "Run Skel: " (skel-shell-calculate-command))
        (alist-get (car (read-multiple-choice "Make dedicated process?"
                                              '((?b "to buffer")
                                                (?p "to project")
                                                (?n "no"))))
                   '((?b . buffer) (?p . project)))
        (= (prefix-numeric-value current-prefix-arg) 4))
     (list (skel-shell-calculate-command)
           skel-shell-dedicated
           t)))
  (let* ((project (and (eq 'project dedicated)
                       (project-current t)))
         (default-directory (if project
                                (project-root project)
                              default-directory))
         (buffer (skel-shell-make-comint
                  (or cmd (skel-shell-calculate-command))
                  (skel-shell-get-process-name dedicated)
                  show)))
    (get-buffer-process buffer)))


(defun project-skel-shell () 
  (interactive)
  (run-skel nil 'project t))

;;; Agenda
;; project agenda integration
;; TODO 2026-06-06: local todo.org files
(defun project-agenda-files ()
  "Return the tasks.org file of the current project."
  (let ((path (join-paths company-org-directory "plan/tasks" (format "%s.org" (project-name (project-current))))))
    (when (file-exists-p path) (list path))))

(defun project-agenda (&optional arg keys restriction)
  (interactive)
  (let ((org-agenda-files (project-agenda-files)))
    (org-agenda arg keys restriction)))

;;; Minor Mode
(define-minor-mode skel-minor-mode
  "skel-minor-mode"
  :global t
  :lighter " Sk"
  :group 'skel
  (keymap-local-set skel-map-prefix skel-map))

;; TODO 2026-05-30: 
;; (defun maybe-skel-minor-mode ()
;;   "Check the current environment and determine if `skel-minor-mode' should
;; be enabled. This function is added as a hook to
;; `lisp-data-mode-hook'.")

;;; Major Mode
;; TODO 2023-09-06: 
(define-derived-mode skel-mode lisp-mode "Skel"
  :group 'skel
  (skel-minor-mode 1)
  (setq-local electric-quote-string t)
  (setq imenu-case-fold-search nil)
  (setq-local indent-region-function 'skel-indent-region)
  (setq-local lisp-indent-offset 1))


;;;###autoload
(defun skel-init ()
  (interactive)
  (mapc (lambda (x) (add-to-list 'auto-mode-alist `(,x . skel-mode))) 
        '("\\.box\\'" "\\.pod\\'" "\\.pkg\\'"
          "\\.?\\(skelrc\\|skelfile\\|sk\\|sxp\\|homerc\\|kryptrc\\|packyrc\\)\\'"))
  (with-eval-after-load 'project 
    (add-to-list 'project-switch-commands '(project-skel-shell "Skel"))
    (add-to-list 'project-switch-commands '(project-agenda "Agenda")))
  (with-eval-after-load 'eglot (add-to-list 'eglot-server-programs '((lisp-mode skel-mode) "skel" "langserver")))
  (with-eval-after-load 'org (org-babel-make-language-alias "skel" "lisp-data")))

;; TODO 2025-10-03: 
;; skel project customization ui (overlays skelfile)

(provide 'skel)
;;; skel.el ends here
