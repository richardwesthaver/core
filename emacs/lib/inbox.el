;;; lib/inbox.el --- Inbox API -*- lexical-binding: t; -*-

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

;;; Code:
(require 'org)

(defgroup inbox nil
  "RW Inbox"
  :group 'rw)

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

;; `org-archive-all-done' doesn't work the way we want. This function
;; will archive all done tasks in the current subtree, or the whole file
;; if prefix arg is given.
(defun org-archive-done (&optional arg)
  "archive all tasks with todo-state of 'DONE' or 'NOPE' in the
current subtree. If prefix arg is given, operate on the entire
file."
  (interactive "P")
  (let ((scope (if arg 'file 'tree)))
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/+DONE|NOPE" scope)))

(defun org-children-done ()
  "Mark all sub-tasks in this heading as 'DONE'."
  (interactive)
  (org-map-entries
   (lambda ()
     (unless (= (org-current-level) 1)
     (org-todo "DONE"))
   nil 'tree)))

(defun org-inbox-migrate ()
  "Migrate all sub-headings to the current week heading, archive
DONE tasks, and delete the empty previous week heading."
  (interactive)
  (let ((scope 'tree)
        (cur (org-inbox-current-week-heading))
        ;; (prev (format-iso-week-number
        ;; (float-time (time-subtract (current-time) (days-to-time 7)))))
        (pos (save-excursion
               (find-file-noselect org-inbox-file)
               (org-find-exact-headline-in-buffer
                (org-inbox-current-week-heading) nil t))))
    (org-archive-done)
    (org-map-entries
     (lambda ()
       (org-refile nil nil (list cur org-inbox-file nil pos))
       (setq org-map-continue-from (org-element-property :begin (org-element-at-point)))
       (setq pos (org-find-exact-headline-in-buffer cur nil t)))
     "LEVEL=2" scope)
    (org-inbox-delete-week-heading)))

(defun org-inbox-week-heading-p ()
  "Check if the heading at point is an org-inbox week heading."
  (let ((hd (org-heading-components)))
    (when (and (eq (car hd) 1)
               (null (caddr hd))
               (null (cadddr hd))
               (string-match "^w[0-9][0-9]$" (nth 4 hd))
               (string-match "^:\\([0-9]\\)\\{4\\}:[A-Z]\\([a-z]\\)\\{2\\}:$" (nth 5 hd)))
      t)))

(defun org-inbox-current-week-p ()
  "Check if the inbox has a heading for current week."
  (let ((buf (find-buffer-visiting org-inbox-file)))
    (unless buf
      (setq buf (find-file-noselect org-inbox-file)))
    (save-excursion
      (with-current-buffer buf
        (goto-char (point-max))
        (if (re-search-backward (concat "^* " (format-iso-week-number)) nil t) t)))))

(defun org-inbox-delete-week-heading ()
  "Delete the week heading at point."
  (interactive)
  (if (not (org-inbox-week-heading-p))
      (if (= (org-current-level) 1)
          (message "Failed to find a week heading at point")
        (progn (org-up-heading-safe)
               (org-inbox-delete-week-heading)))
    (progn (org-mark-subtree)
           (delete-region (region-beginning) (region-end)))))

(defun org-inbox-insert-week-heading ()
  "Insert a new heading for the current week.
Format:
* w01 :2023:Jan:
  SCHEDULED: <2023-01-02 Mon>--<2023-01-09 Sun>
"
  (interactive)
  (let ((buf (find-buffer-visiting org-inbox-file)))
    (unless buf
      (setq buf (find-file-noselect org-inbox-file)))
    (save-excursion
      (with-current-buffer buf
        (goto-char (point-max))
        (org-previous-visible-heading 1)
        (while (> (org-outline-level) 1)
          (outline-up-heading 1))
        (let* ((fmt org-inbox-date-start-format)
               (date-start
                (time-add
                 (org-timestamp-to-time
                  (org-timestamp-from-string
                   (plist-get
                    (cadr (org-element-at-point))
                    :DATE_START)))
                 (days-to-time 7)))
               (date-end (format-time-string fmt (last-day-of-week date-start)))
               (title (format-iso-week-number date-start))
               (elt (org-element-interpret-data
                     `(headline
                       (:title ,title :level 1 :tags (,(format-time-string "%Y:%b" date-start)))
                       (property-drawer nil
                                        ((node-property
                                          (:key "DATE_START" :value ,(format-time-string fmt date-start)))))))))
          (goto-char (point-max))
          (newline)
          (insert elt)
          title)))))

(defun org-inbox-current-week-heading ()
  "Find the location of the current week heading in
  `org-inbox-file'. Create it if it doesn't exist."
  (if (org-inbox-current-week-p)
      (format-iso-week-number)
    (org-inbox-insert-week-heading)))

(defun org-sort-todo-priority ()
  "Sorting function used by `org-sort' to sort by todo order
    followed by priority. Returns a pair of numbers (TODO . PRIO)."
  (let* ((elt (cadr (org-element-at-point)))
         (todo (substring-no-properties (plist-get elt :todo-keyword)))
         (prio (plist-get elt :priority))
         (res))
    (message "%s %s" todo prio)
    (unless prio (setq prio 5))
    ;; FIXME todo states shouldn't be hardcoded
    (cond
     ((string= todo "GOTO") (setq res (cons 1 prio)))
     ((string= todo "TODO") (setq res (cons 2 prio)))
     ((string= todo "WAIT") (setq res (cons 3 prio)))
     ((string= todo "HOLD") (setq res (cons 4 prio)))
     ((string= todo "DONE") (setq res (cons 5 prio)))
     ((string= todo "NOPE") (setq res (cons 6 prio))))
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
     ((> (cdr a) (cdr b)) nil)
     ;; nil ommitted since cond defaults to it
     ))))
(defun org-inbox-sort-week ()
  "Sort the current heading by todo order followed by priority."
  (interactive)
  (org-sort-entries nil ?f #'org-sort-todo-priority #'org-sort-compare-todo-priority))

