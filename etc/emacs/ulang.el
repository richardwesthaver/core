;;; ulang.el --- ulang compliance lib -*- lexical-binding:t -*-

;; Copyright (C) 2023  The Compiler Company

;; Author: <ellis@compiler.company>
;; Keywords: comm

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
(require 'org)
(require 'ox)
(require 'rx)

;;; Custom
(defgroup ulang nil "ULANG")

(defcustom ulang-properties
  '("VERSION" "LOCATION" "PRONOUNCE" "AKA" "CREATED" "ALLOC")
  nil
  :group 'ulang)

(defcustom ulang-info-url-alist
  '(("sbcl" . "https://www.sbcl.org/manual/")
    ("asdf" . "https://asdf.common-lisp.dev/asdf/")
    ("tar" . "https://www.gnu.org/software/tar/manual/")
    ("emms" . "https://www.gnu.org/software/emms/manual/")
    ("clang" . "https://clang.llvm.org/docs/UsersManual.html")
    ("llvm" . "https://llvm.org/docs/ProgrammersManual.html")
    ("slime" . "https://slime.common-lisp.dev/doc/html/")
    ("gforth" . "https://www.complang.tuwien.ac.at/forth/gforth/Docs-html/")
    ("ecl" . "https://ecl.common-lisp.dev/static/manual/")
    ("notmuch" . "https://notmuchmail.org/doc/latest/man1/")
    ("guile" . "https://www.gnu.org/software/guile/manual/html_node/")
    ("chromium" . "https://developer.chrome.com/docs/chromium/"))
  "See 'org-info-other-documents'."
  :group 'ulang)

(defcustom ulang-link-abbrev-alist
  '()
  "See `org-link-abbrev-alist'."
  :group 'ulang)

(defcustom ulang-created-property-name "CREATED"
  "The name of the property for setting the creation date."
  :type 'string
  :group 'ulang)

(defcustom ulang-created-date "+0d"
  "The default creation date.
The default value of this variable (\"+0d\") means that entries
without a creation date will be handled as if they were created
today.

If the creation date cannot be retrieved from the entry or the
subtree above, the expiry process will compare the expiry delay
with this date.  This can be either an ISO date or a relative
time specification.  See `org-read-date' for details on relative
time specifications."
  :type 'string
  :group 'org-expire)

(defcustom ulang-inactive-timestamps nil
  "Insert inactive timestamps for created/expired properties."
  :type 'boolean
  :group 'ulang)

(defcustom ulang-todo-keywords '("TODO" "REVIEW" "FIX" "HACK" "RESEARCH" "NOTE")
  "See `org-todo-keywords-for-agenda'."
  :group 'ulang)

(defface ulang-todo '((t :inherit org-todo))
  "Default face used for `ulang-todo-keywords' in ulang-minor-mode."
  :group 'ulang)

(defcustom ulang-todo-keyword-faces nil
  "See `org-todo-keyword-faces'."
  :group 'ulang)

(defcustom ulang-export-dictionary
  (list '("Table of Contents" "⇝"))
  "See 'org-export-dictionary'."
  :group 'ulang)

(defcustom ulang-agenda-commands
  '(("d" "Daily Agenda" 
     ((tags-todo "+PRIORITY=\"A\"" ((org-agenda-overriding-header "High Priority Tasks")))
      (agenda "" ((org-agenda-span 'day)))))
    ("w" "Weekly Review"
     ((agenda ""
              ((org-agenda-overriding-header "Incomplete Tasks")
               (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
               (org-agenda-span 'week)))
      (agenda ""
              ((org-agenda-overriding-header "Completed Tasks")
               (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'done))
               (org-agenda-span 'week)))))
    ("i" "Work in progress tasks" ((todo "WIP" ((org-agenda-sorting-strategy '(priority-up effort-down))))))
    ("c" "Core tasks" ((tags-todo "+core" ((org-agenda-sorting-strategy '(priority-up effort-down))))))
    ("u" "Untagged tasks" ((tags-todo "-{.*}" ((org-agenda-sorting-strategy '(priority-up effort-down))))))
    (" " "Inbox"
     ((tags-todo ".*" ((org-agenda-files `(,org-inbox-file))
                       (org-agenda-overriding-header "Inbox Items")
                       (org-agenda-sorting-strategy '(priority-up effort-down))))
      (agenda "" ((org-agenda-span 'day))))))
  "See `org-agenda-custom-commands'."
  :group 'ulang)

(defvar ulang-syntax-table (copy-syntax-table text-mode-syntax-table)
  "Syntax table used by `ulang-minor-mode'.")

;;; Regexps
(defvar default-line-regexp-alist
  '((empty . "[\s\t]*$")
    (indent . "^[\s\t]+")
    (non-empty . "^.+$")
    (list . "^\\([\s\t#*+]+\\|[0-9]+[^\s]?[).]+\\)")
    (heading . "^[=-]+"))
  "Alist of regexp types used by `default-line-regexp-p'.")

(defun default-line-regexp-p (type &optional n)
  "Test for TYPE on line.
TYPE is the car of a cons cell in
`default-line-regexp-alist'.  It matches a regular
expression.
With optional N, search in the Nth line from point."
  (save-excursion
    (goto-char (pos-bol))
    (and (not (bobp))
     (or (beginning-of-line n) t)
     (save-match-data
       (looking-at
        (alist-get type default-line-regexp-alist))))))

;;; Time
(defun format-iso-week-number (&optional date)
  "format DATE as ISO week number with week days starting on
    Monday. If DATE is nil use current date."
  (let* ((week (format-time-string "%W" date))
     (prefix (if (= (length week) 1)
             "w0" "w")))
    (concat prefix week)))

(defun last-day-of-year (&optional date)
  "Return the last day of the year as time."
  (encode-time 0 0 0 31 12 (nth 5 (decode-time
                   (or date (current-time))))))

(defun last-day-of-month (&optional date)
  "Return the last day of month as time."
  (let* ((now (decode-time (or date (current-time))))
     (month (nth 4 now))
     (year (nth 5 now))
     (last-day-of-month (calendar-last-day-of-month month year)))
    (encode-time 0 0 0 last-day-of-month month year)))

(defun last-day-of-week (&optional date)
  "Return the last day of the week as time."
  (let* ((now (or date (current-time)))
     (datetime (decode-time now))
     (dow (nth 6 datetime)))
    (time-add now (days-to-time (- 7 dow)))))

(defun first-day-of-week (&optional date)
  "Return the first day of the week as time."
  (let* ((now (or date (current-time)))
     (datetime (decode-time now))
     (dow (nth 6 datetime)))
    (time-subtract now (days-to-time dow))))

;;; Hooks
;;;###autoload
(defun ulang--org-page-delimiter ()
  (setq-local page-delimiter "^\\(\\|\\* \\)"))
;;;###autoload
(defun ulang--lisp-page-delimiter ()
  (setq-local page-delimiter "^\\(\\|;;; \\)"))
;;;###autoload
(defun ulang--sh-page-delimiter ()
  (setq-local page-delimiter "^\\(\\|### \\)"))

;;; Utils
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

;;;###autoload
(defun ulang-init ()
  (interactive)
  (org-babel-lob-ingest company-babel-file)
  (org-export-translate-to-lang ulang-export-dictionary "ulang")
  (mapadd org-info-other-documents ulang-info-url-alist)
  (mapadd browse-url-filename-alist ulang-info-url-alist)
  (mapadd org-link-abbrev-alist ulang-link-abbrev-alist)
  ;; ulang-properties ?
  (mapadd org-agenda-custom-commands ulang-agenda-commands)
  (mapadd org-todo-keywords-for-agenda ulang-todo-keywords)
  (mapadd org-todo-keyword-faces ulang-todo-keyword-faces)
  (add-hook 'org-insert-heading-hook 'org-insert-created)
  (add-hook 'org-after-todo-state-change-hook 'org-insert-created)
  (add-hook 'org-after-tags-change-hook 'org-insert-created))

(defun ulang-deinit ()
  (remove-hook 'org-insert-heading-hook 'org-insert-created)
  (remove-hook 'org-after-todo-state-change-hook 'org-insert-created)
  (remove-hook 'org-after-tags-change-hook 'org-insert-created))

;;; Location
;; (org-property-inherit-p "LOCATION")

;; currently does not support locations with spaces.. need to walk
;; ancestors ourselves to do so. for now only URIs and pathnames are
;; supported.
(defun org-get-with-inheritance (property &optional literal-nil epom)
  "Like `org-entry-get-with-inheritance' but in additional to properties we
also check file keywords (aka in-buffer settings).

For example, a PROPERTY value of 'LOCATION' would check all property
values in addition to the keyword '#+LOCATION:'."
  (interactive (list nil nil))
  (let* ((property (or property (org-read-property-name)))
         (kw (when-let* ((val (org-collect-keywords '("LOCATION") nil)))
               (cadar val)))
         ;; most of the work passed through to the property handler
         (props (org-entry-get-with-inheritance property literal-nil epom)))
    (if kw
        (append (list kw) (if (listp props) props (list props)))
      props)))

(defun org-get-location (point)
  "Get the value of property LOCATION at POINT."
  (interactive "d")
  (let ((path (org-get-with-inheritance "LOCATION" nil point)))
    ;; when the second path component is an absolute path, skip the first
    (when (and (< 1 (length path)) (file-name-absolute-p (cadr path)))
      (setq path (cdr path)))
    (message "%s"
             (apply 'join-paths
                    (flatten
                     (mapcar
                      (lambda (x) (split-string x " "))
                      path))))))

(defun org-set-location (value)
  "Set the value of property LOCATION. If point is before first heading
instead set or replace the location file keyword."
  (interactive (list nil))
  (let ((val (or value (org-read-property-value "LOCATION" nil nil))))
    (if (org-before-first-heading-p)
        (save-excursion
          (goto-char (point-min))
          (let ((start (point)))
            (when (re-search-forward (rx bol "#+LOCATION:" (+ space) (group (* (not space))) eol) nil t)
              (setq start (match-beginning 0))
              (goto-char start)
              (delete-line))
            (insert "#+LOCATION: " val "\n")))
      (org-set-property "LOCATION" value))))

(defun org-follow-location (point &optional arg)
  "Open the location specified by the LOCATION property of the org heading
or file at point. With C-u or ARG open in separate window."
  (interactive "d")
  (let ((loc (org-get-location point))
        (arg (or arg current-prefix-arg)))
    (cond
     ((string-match-p org-link-any-re loc) (org-link-open-from-string loc))
     ;; TODO 2024-08-29: handle other location types (physical, etc)
     (t (funcall (if arg 'find-file-other-window 'find-file) loc t)))))

;;; Created
(defun ulang--format-timestamp (timestr inactive)
  "Properly format TIMESTR into an org (in)active timestamp"
  (format (if inactive "[%s]" "<%s>") timestr))

(defun org-insert-created (&optional arg)
  "Insert or update a property with the creation date.
If ARG, always update it.  With one `C-u' prefix, silently update
to today's date.  With two `C-u' prefixes, prompt the user for to
update the date."
  (interactive "P")
  (let* ((d (org-entry-get (point) ulang-created-property-name))
         d-time d-hour timestr)
    (when (or (null d) arg)
      ;; update if no date or non-nil prefix argument
      (setq d-time (if d (org-time-string-to-time d)
             (current-time)))
      (setq d-hour (format-time-string "%H:%M" d-time))
      (setq timestr
        ;; two C-u prefixes will call org-read-date
            (ulang--format-timestamp
             (if (equal arg '(16))
                 (org-read-date nil nil nil nil d-time d-hour)
               (format-time-string
                (replace-regexp-in-string "\\(^<\\|>$\\)" ""
                                          (cdr org-time-stamp-formats))))
             ulang-inactive-timestamps))
      (save-excursion
    (org-entry-put
     (point) ulang-created-property-name timestr)))))

;;; Comments
;; see also [[https://github.com/tarsius/hl-todo/blob/main/hl-todo.el][hl-todo.el]]
(defun ulang-comment-keywords () 
  "Parse 'ulang-todo-keywords' and return a list of simplified todo keywords."
  (cl-remove-duplicates
   (append 
    ulang-todo-keywords
    (mapcan 
     (lambda (x)
       (mapcar (lambda (y) (car (string-split y "(" nil)))
               (remove "|" x)))
     (mapcar 'cdr org-todo-keywords)))
   :test 'string=))

(defcustom ulang-comment-timestamp-format-concise "%F"
  "Specifier for date in `ulang-comment-timestamp-keyword'.
Refer to the doc string of `format-time-string' for the available
options."
  :group 'ulang
  :type 'string)

(defcustom ulang-comment-timestamp-format-verbose "%F %T %z"
  "Like `ulang-comment-timestamp-format-concise', but longer."
  :group 'ulang
  :type 'string)

;;;###autoload
(defun ulang-comment-dwim (arg)
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

(defvar ulang-comment--keyword-hist '()
  "Input history of selected comment keywords.")

(defun ulang-comment--keyword-prompt (keywords)
  "Prompt for candidate among KEYWORDS."
  (let ((def (car ulang-comment--keyword-hist)))
    (completing-read
     (format "Select keyword [%s]: " def)
     keywords nil nil nil 'ulang-comment--keyword-hist def)))

;;;###autoload
(defun ulang-comment-timestamp-keyword (keyword &optional verbose)
  "Add timestamped comment with KEYWORD.

When called interactively, the list of possible keywords is that
of `ulang-todo-keywords', though it is possible to
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
`ulang-comment-timestamp-format-concise'.

With optional VERBOSE argument (such as a prefix argument
`\\[universal-argument]'), use an alternative date format, as
specified by `ulang-comment-timestamp-format-verbose'."
  (interactive
   (list
    (ulang-comment--keyword-prompt (ulang-comment-keywords))
    current-prefix-arg))
  (let* ((date (if verbose
                   ulang-comment-timestamp-format-verbose
                 ulang-comment-timestamp-format-concise))
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

;;; ulang-minor-mode
(defcustom ulang-minor-mode-fontify t
  "When Non-nil fontify 'ulang-minor-mode' buffers."
  :type 'boolean
  :group 'ulang
  :local t)

(defcustom ulang-minor-mode-use-readtable nil
  "When Non-nil use embedded organ readtable syntax in 'ulang-minor-mode' buffers."
  :type 'boolean
  :group 'ulang
  :local t)

;; support ORG reader syntax in lisp files 
;;;###autoload
(defun org-links-in-buffer (&optional start end)
"Return a list of org-links as (FULL BEG END LINK BEG END DESC BEG END) in the current buffer."
  (let ((matches))
    (save-match-data
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (or start (point-min)))
          (while (search-forward-regexp org-link-any-re end t 1)
            (push
             (if (org-in-regexp org-link-bracket-re)
                 (list
                  ;; start/end
                  (match-string-no-properties 0)
                  (match-beginning 0)
                  (match-end 0)
                  ;; 1
                  (match-string-no-properties 1) 
                  (match-beginning 1)
                  (match-end 1)
                  ;; 2
                  (match-string-no-properties 2)
                  (match-beginning 2)
                  (match-end 2))
               (list (match-string-no-properties 0)
                     (match-beginning 0)
                     (match-end 0)))
             matches))))
      matches)))

;;;###autoload
(defun ulang-comment-keywords-in-buffer (&optional start end)
"Return a list of comment keywords in buffer as (KW START END TS START
END DESC START END) in the current buffer."
  (let ((matches))
    (save-match-data
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (or start (point-min)))
          (while (search-forward-regexp  
                  (rx-let ((ulang-comment-keyword (eval `(or ,@(ulang-comment-keywords)))))
                    (rx (* space) (+ (eval (or comment-start ";"))) (+ space)
                        (group ulang-comment-keyword)
                        (? 
                         (seq
                          (* space)
                          (group (* not-newline) digit)))
                        ":"
                        (? (seq
                            (* space)
                            (group (* not-newline))))))
                  end t 1)
            (push
             (list
              (match-string-no-properties 1)
              (match-beginning 1)
              (match-end 1)
              (match-string-no-properties 2) 
              (match-beginning 2)
              (match-end 2)
              (match-string-no-properties 3) 
              (match-beginning 3)
              (match-end 3))
             matches))))
      matches)))

;;;###autoload
(defun ulang-minor-mode-swap-setup ()
  (make-local-variable 'post-command-hook)
  (add-hook 'post-command-hook 'ulang-minor-mode-swap nil t))

;;;###autoload
(defun ulang-fontify-region (&optional start end verbose)
  (let ((start (or start (point-min)))
        (end (or end (point-max))))
    (font-lock-default-fontify-region start end verbose)
    (mapc (lambda (a)
            (cl-destructuring-bind (x xs xe &optional y ys ye z zs ze) a
              (make-button
               xs xe
               'data (or y x)
               'action 'org-open-at-point-global
               'help-echo (or (when z (format "%s (%s)" z y)) x))
              (if z 
                  (progn
                    (put-text-property xs zs 'invisible t)
                    (put-text-property ze xe 'invisible t)))))
          (org-links-in-buffer start end))
    (mapc (lambda (b)
            (cl-destructuring-bind (x xs xe y ys ye z zs ze) b
              (when (or y (not (string-empty-p z))) (put-text-property xs xe 'face (org-get-todo-face x))
                    (when y (put-text-property ys ye 'face 'org-agenda-date))
                    (when (not (string-empty-p z)) (put-text-property zs ze 'face 'org-scheduled)))))
          (ulang-comment-keywords-in-buffer start end))))

(defun ulang-minor-mode-link-setup ()
  (setq-local font-lock-fontify-region-function 'ulang-fontify-region))

;; FIX 2026-05-01: readtable regexps
(defun ulang-minor-mode-swap ()
  (let ((lm -1)
        (rm -1)
        (vbar nil)
        (p (point)))
    (save-excursion 
      (if (or (search-backward "#&" nil t)
              (and (re-search-backward "#|[ ]?org" nil t) (setf vbar t)))
          (setq lm (point))
        (setq lm -1)))
    (save-excursion
      (if (or (and (not vbar) (search-forward "&#" nil t))
              (search-forward "|#" nil t))
          (setq rm (point))
        (setq rm -1)))
    (if (< 0 lm p rm)
        (progn (major-mode-suspend) (org-mode))
      (major-mode-restore))
    (ulang-minor-mode-swap-setup)))

(define-minor-mode ulang-minor-mode nil
  :lighter " ulang"
  :group 'ulang
  :interactive (prog-mode)
  ;; (if (derived-mode-p 'lisp-mode) (setq-local ulang-minor-mode-use-readtable t))
  (when ulang-minor-mode-use-readtable (ulang-minor-mode-swap-setup))
  (when ulang-minor-mode-fontify (ulang-minor-mode-link-setup)))

(provide 'ulang)
;;; ulang.el ends here
