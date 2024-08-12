;;; ulang.el --- ulang compliance lib -*- lexical-binding:t -*-

;; Copyright (C) 2023  

;; Author:  <ellis@zor>
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
;; (setq org-export-global-macros nil)

;;; Code:
(require 'org)
(require 'ox)

(defvar ulang-links-history nil)
(defvar ulang-files-history nil)

;;;###autoload
(defun ulang-dblock-insert-links (regexp)
  "Create dblock to insert links matching REGEXP."
  (interactive (list (read-regexp "Insert links matching: " nil ulang-links-history)))
  (org-create-dblock (list :name "links"
                           :regexp regexp
                           :id-only nil))
  (org-update-dblock))

(org-dynamic-block-define "links" 'ulang-dblock-insert-links)

(org-export-translate-to-lang (list '("Table of Contents" "TOC")) "ulang")

(setq org-todo-keywords
      '((type "TBD(0!)" "TODO(t!)" "|")
        (type "WIP(w!)" "|")
        (type "HOLD(H@!)" "WAIT(/j@!)" "|")
        (sequence "FIND(q!)" "READ(r@!)" "WATCH(W@!)" "|")
        (sequence "RESEARCH(s!)" "RECORD(e!)" "|")
        (sequence "OUTLINE(O!)" "RESEARCH(A!)" "DRAFT(M!)" "REVIEW(R!)" "|")
        (sequence "FIX(i!)" "TEST(t!)" "|")
        (type "GOTO(g!)" "HACK(h!)" "NOTE(n!)" "CODE(c!)" "LINK(l!)" "|")
        (type "PROJECT(p!)" "|")
        (type "KLUDGE(k@!)" "|")
        (sequence "|" "DONE(d!)" "NOPE(x@!)" "FOUND(f@!)")))

(setq org-todo-keyword-faces
      '(("PROJECT" . (:foreground "lightseagreen" :weight bold))
        ("NOTE" . (:foreground "lemonchiffon" :weight bold))))

(setq org-link-abbrev-alist
      '(("vc" . "https://vc.compiler.company/%s")
        ("comp" . "https://compiler.company/%s")
	("cdn" . "https://cdn.compiler.company/%s")
        ("packy" . "https://packy.compiler.company/%s")
        ("yt" . "https://youtube.com/watch?v=%s")))

;;; IDs
(defun org-custom-id-get (&optional pom create prefix)
  "Get the CUSTOM_ID property of the entry at point-or-marker POM.
   If POM is nil, refer to the entry at point. If the entry does
   not have an CUSTOM_ID, the function returns nil. However, when
   CREATE is non nil, create a CUSTOM_ID if none is present
   already. PREFIX will be passed through to `org-id-new'. In any
   case, the CUSTOM_ID of the entry is returned."
  (interactive)
(org-with-point-at pom
  (let ((id (org-entry-get nil "CUSTOM_ID"))
        ;; use CUSTOM_ID for links
        (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       (create
        (setq id (org-id-new prefix))
        (org-entry-put pom "CUSTOM_ID" id)
        (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
        id)))))

;;;###autoload
(defun org-id-add-to-headlines-in-file ()
  "Add ID properties to all headlines in the
   current file which do not already have one."
  (interactive)
  (org-map-entries (lambda () (org-id-get (point) 'create))))

(defun org-custom-id-add-to-headlines-in-file ()
  "Add CUSTOM_ID properties to all headlines in the
   current file which do not already have one."
  (interactive)
  (org-map-entries (lambda () (org-custom-id-get (point) 'create))))

(defun org-id-add-to-headlines-in-agenda-files ()
  (interactive)
  (with-temp-buffer
    (dolist (f org-agenda-files)
      (find-file f)
      (org-id-add-to-headlines-in-file)
      (save-buffer))))

(message "Initialized ULANG.")

(provide 'ulang)
;;; ulang.el ends here
