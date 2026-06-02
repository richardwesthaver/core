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
(require 'org-expire)

(defgroup inbox nil
  "CC Inbox"
  :group 'org)

;;; Custom
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

(defcustom org-inbox-buffer-name "*inbox*"
  "The name of the org-inbox buffer."
  :group 'inbox)

(defcustom org-inbox-config-buffer-name "*inbox-config*"
  "Then name of the org-inbox configuration buffer."
  :group 'inbox)

(defcustom org-inbox-buffer-name "*Inbox*"
  "The name of the org-inbox buffer."
  :group 'inbox)

;;; Variables
(defvar org-inbox-properties
  '("NEXT" "PREV" "FROM" "TO" "OWNER" "PROJECT" "BLOCKS" "VERSION"))

(defvar org-inbox-db-schema
  '(id file node edge contents properties schedule))

(defcustom org-inbox-capture-templates
  `(("i" "inbox-item" entry (file ,org-inbox-file)
     "%i"
     :unnarrowed t
     :clock-keep t
     :empty-lines 1
     :prepare-finalize (org-id-get-create org-expire-insert-created))
    ("t" "inbox-task" entry (file ,org-inbox-file) "%i\n"
     :empty-lines-before 1
     :prepare-finalize (org-id-get-create org-expire-insert-created))
    ("n" "inbox-note" entry (file ,org-inbox-file) "%i\n%a"
     :empty-lines-before 1
     :prepare-finalize (org-id-get-create org-expire-insert-created))
    ("l" "log" entry (file+olp+datetree ,(expand-file-name "log.org" org-directory)) "%i" 
     :empty-lines-before 1 
     :unnarrowed t
     :prepare-finalize (org-id-get-create org-expire-insert-created)))
  "List of additional capture templates loaded by 'inbox'."
  :group 'inbox)

(defmacro with-inbox-buffer (&rest body)
  `(save-excursion
     (with-current-buffer (find-file org-inbox-file)
       ,@body)))

(defun org-sort-todo-priority ()
  "Sorting function used by `org-sort' to sort by todo order
    followed by priority. Returns a pair of numbers (TODO . PRIO)."
  (interactive)
  (let* ((elt (cadr (org-element-at-point)))
         (todo (when-let* ((kw (plist-get elt :todo-keyword)))
                 (when (stringp kw)
                   (substring-no-properties kw))))
         (prio (pcase (plist-get elt :priority)
                 ("A" 1)
                 ("B" 2)
                 ("C" 3)
                 (_ 4)))
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
     ((string= todo "DONE") (setq res (cons 5 prio)))
     ((string= todo "NOPE") (setq res (cons 5 prio))))
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
   (org-map-entries #'org-expire-insert-created)
   (org-inbox-sort)))

(defun org-inbox-open ()
  "Open `org-inbox-file' or switch to its buffer if already open."
  (interactive)
  (if-let* ((inbox (get-buffer org-inbox-buffer-name)))
      (switch-to-buffer inbox)
    (find-file org-inbox-file)
    (rename-buffer org-inbox-buffer-name)))

(defun org-inbox-close ()
  "Close the org-inbox and associated buffers."
  (interactive)
  (when-let* ((inbox (get-buffer org-inbox-buffer-name)))
    (kill-buffer inbox)))

;;;###autoload
(defun org-inbox-init ()
  (mapadd org-capture-templates org-inbox-capture-templates)
  (mapadd org-special-properties org-inbox-properties))

(provide 'inbox)
;; inbox.el ends here
