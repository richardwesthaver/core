;;; lib/inbox.el --- Inbox API -*- lexical-binding: t -*-

;; Copyright (C) 2023  Richard Westhaver
;; Version: "0.2.0"
;; Keywords: maint, tools, outlines, extensions

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

;; This is the elisp interface to the CC Inbox system. The main
;; interface is the inbox.org file which manages personal tasks.

;; Users may use `org-capture' to insert tasks and notes into their
;; own `org-inbox-file' and refactor them to a more sensible
;; destination with `org-refile'.

;;; Code:
(require 'org)
(require 'org-agenda)
(require 'default)
;; (require 'uml-mode)
(require 'org-expiry)

(defgroup inbox nil
  "CC Inbox")

;;; Vars
(defcustom org-inbox-file
  (concat (file-name-as-directory org-directory) "inbox.org")
  "Custom inbox file location."
  :type 'file
  :group 'inbox)

(defcustom org-inbox-date-start-format "<%Y-%m-%d %a>"
  "Format of DATE_START property timestamp for week headings. See
`org-time-stamp-formats' for accepted values."
  :type 'string
  :group 'inbox)

(defvar org-inbox-buffer-name "*Inbox*"
  "The name of the org-inbox buffer.")

(defvar org-inbox-config-buffer-name "*Inbox Config*"
  "Then name of the org-inbox configuration buffer.")

(defvar org-inbox-properties
  '("NEXT" "PREV" "FROM" "TO" "OWNER" "PROJECT" "BLOCKER" "VERSION"))

(defvar org-inbox-db-schema
  '(id file node edge contents properties schedule))

;;; Capture
(setq org-id-link-to-org-use-id t
      org-protocol-default-template-key "L")

;; capture templates
(setq org-capture-templates
      `(("i" "inbox-item" entry (file ,org-inbox-file)
         "* %?\n%i"
         :empty-lines 1)
        ("t" "inbox-task" entry (file ,org-inbox-file) "* TODO %^{item}\n")
        ("n" "inbox-note" entry (file ,org-inbox-file) "* NOTE %^{item}\n%a")
        ("l" "inbox-link" entry (file ,org-inbox-file)
         "* LINK %l")
        ("L" "inbox-protocol-link" entry (file ,org-inbox-file)
         "* LINK [[%:link][%:description]]\n%:initial" :empty-lines 1)
        ("w" "inbox-web-link" entry (file ,org-inbox-file)
         "* LINK %?"
         :hook (lambda ()
                 (goto-char (pos-eol))
                 (org-web-tools-insert-link-for-url (org-web-tools--get-first-url))))
        ("1" "current-task-item" item (clock) "%i%?")
        ("2" "current-task-checkbox" checkitem (clock) "%i%?")
        ("3" "current-task-region" plain (clock) "%i" :immediate-finish t :empty-lines 1)
        ("4" "current-task-kill" plain (clock) "%c" :immediate-finish t :empty-lines 1)
        ("l" "log" item (file+headline "log.org" "log") "%U %?" :prepend t)
        ("s" "secret" table-line (file+function "krypt" org-ask-location) "| %^{key} | %^{val} |" :immediate-finish t :kill-buffer t)
        ("N" "note-item" plain (file+function "notes.org" org-ask-location) "%?")))

(add-hook 'org-after-todo-state-change-hook #'org-id-get-create)
(add-hook 'org-after-todo-state-change-hook #'org-expiry-insert-created)

(setq org-default-notes-file (join-paths org-directory "inbox.org")
      org-capture-use-agenda-date t
      org-archive-location "archive.org::")

;;; Utils
;; `org-archive-all-done' doesn't work the way we want. This function
;; will archive all done tasks in the current subtree, or the whole file
;; if prefix arg is given.
(defun org-archive-done (&optional scope)
  "archive all tasks with todo-state of 'DONE' or 'NOPE'."
  (interactive "P")
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/+DONE|NOPE" scope))

(defun org-children-done ()
  "Mark all sub-tasks in this heading as 'DONE'."
  (interactive)
  (org-map-entries
   (lambda ()
     (unless (= (org-current-level) 1)
     (org-todo "DONE"))
   nil 'tree)))

(defmacro with-inbox-buffer (&rest body)
  `(save-excursion
     (with-current-buffer (find-file org-inbox-file)
       ,@body)))

(defun org-sort-todo-priority ()
  "Sorting function used by `org-sort' to sort by todo order
    followed by priority. Returns a pair of numbers (TODO . PRIO)."
  (let* ((elt (cadr (org-element-at-point)))
         (todo (when-let ((kw (plist-get elt :todo-keyword)))
                 (when (stringp kw)
                   (substring-no-properties kw))))
         (prio (pcase (plist-get elt :priority)
                 ("A" 1)
                 ("B" 2)
                 ("C" 3)
                 (t 2)))
         (res))
    ;; FIXME todo states shouldn't be hardcoded
    (cond
     ((null todo) (setq res (cons 3 prio)))
     ((string= todo "WATCH") (setq res (cons 3 prio)))
     ((string= todo "WAIT") (setq res (cons 1 prio)))
     ((string= todo "HOLD") (setq res (cons 1 prio)))
     ((string= todo "WIP") (setq res (cons 1 prio)))
     ((string= todo "GOTO") (setq res (cons 2 prio)))
     ((string= todo "TODO") (setq res (cons 2 prio)))
     ((string= todo "RESEARCH") (setq res (cons 3 prio)))
     ((string= todo "DONE") (setq res (cons 4 prio)))
     ((string= todo "NOPE") (setq res (cons 4 prio))))
    (unless res (setq res (cons 0 prio)))
    res))

(defun org-sort-compare-todo-priority (a b)
  "Given two cons consisting of (TODO . PRIO), return t if A
  should come before B."
  (message "a: %S b: %S" a b)
  (cond
   ((< (car a) (car b)) t)
   ((> (car a) (car b)) nil)
   ((= (car a) (car b))
    (cond
     ((< (cdr a) (cdr b)) t)
     ((> (cdr a) (cdr b)) nil)))))
     

(defun org-inbox-sort ()
  "Sort the current heading by todo order followed by priority."
  (interactive)
  (with-inbox-buffer
   (org-sort-entries nil ?f #'org-sort-todo-priority #'org-sort-compare-todo-priority)))

(defun org-inbox-compact ()
  "Assign missing IDs and creation dates, archive DONE tasks."
  (interactive)
  (with-inbox-buffer
   (org-id-update-id-locations)
   (org-id-add-to-headlines-in-file)
   (org-archive-done)
   (org-map-entries #'org-expiry-insert-created)
   (org-inbox-sort)))

(defun org-inbox-open ()
  "Open `org-inbox-file' or switch to its buffer if already open."
  (interactive)
  (if-let ((inbox (get-buffer org-inbox-buffer-name)))
      (switch-to-buffer inbox)
    (find-file org-inbox-file)
    (rename-buffer org-inbox-buffer-name)))

(defun org-inbox-close ()
  "Close the org-inbox and associated buffers."
  (interactive)
  (when-let ((inbox (get-buffer org-inbox-buffer-name)))
    (kill-buffer inbox)))

;;; dblocks
(defun org-dblock-write:summary ())

(defun org-inbox-configure-dblock ()
  "Configure the current org-inbox-dblock at point."
  (interactive)
  (with-demoted-errors "Error: %S"
    (let* ((beginning (org-beginning-of-dblock))
           (parameters (org-prepare-dblock)))
      (org-inbox-show-config-buffer (current-buffer) beginning parameters))))

;;; ui
(defun org-inbox-show-config (&optional buffer position parameters)
  (interactive)
  (switch-to-buffer org-inbox-config-buffer-name)
  (erase-buffer)
  (remove-overlays)
  (widget-insert "\n\n")
    (widget-create 'push-button
      :notify (lambda(_widget &rest _ignore)
                (with-current-buffer buffer
                  (goto-char position)
                  )
                (kill-buffer)
                (org-ctrl-c-ctrl-c))
      (propertize "Apply" 'face 'font-lock-comment-face))
    (widget-insert " ")
    (widget-create 'push-button
      :notify (lambda (_widget &rest _ignore)
                (kill-buffer))
      (propertize "Cancel" 'face 'font-lock-string-face))
  (use-local-map widget-keymap)
  (widget-setup))

(provide 'inbox)
;; inbox.el ends here
