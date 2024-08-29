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


;;; Code:
(require 'org)
(require 'ox)

(defgroup ulang nil
  "CC Universal Language.")

(defvar ulang-link-history nil)
(defvar ulang-file-history nil)

(defvar ulang-extra-properties
  '("VERSION"))

;;;###autoload
(defun dblock-insert-links (regexp)
  "Create dblock to insert links matching REGEXP."
  (interactive (list (read-regexp "Insert links matching: " nil ulang-links-history)))
  (org-create-dblock (list :name "links"
                           :regexp regexp
                           :id-only nil))
  (org-update-dblock))

(org-dynamic-block-define "links" 'dblock-insert-links)

(org-export-translate-to-lang (list '("Table of Contents" "Index")) "ulang")
;; (setq org-export-global-macros nil)

;; todo keywords
(setq org-stuck-projects '("+PROJECT/-DONE" ("NEXT") nil ""))

(setq org-todo-keywords
      '((sequence "TBD(0!)" "TODO(t!)" "NEXT(n!)" "WIP(i!)" "|" "DONE(d!)")
        (sequence "HOLD(H@/!)" "WIP(!)" "|")
        (sequence "WAIT(W@/!)" "WIP(!)" "|")
        (sequence "RESEARCH(s!)" "WIP(!)" "REPORT(c!)" "|")
        (sequence "OUTLINE(O!)" "DRAFT(M!)" "REVIEW(V!)" "|")
        (sequence "FIXME(f!)" "WIP(!)" "TEST(T!)" "|")
        (type "FIND(q!)" "READ(r@!)" "WATCH(A@!)" "HACK(h!)"
              "CODE(c!)" "BENCH(b!)" "DEPLOY(D!)" "RUN(X!)"
              "REFILE(w!)" "LOG(L!)" "GOTO(g!)" "|")
        (type "PROJECT(p!)" "PRODUCT(P!)" "SPRINT(S!)" "RELEASE(R!)" "|")
        (sequence "|" "DONE(d!)" "NOPE(x@!)")))

(setq org-todo-keyword-faces
      '(("PROJECT" . (:foreground "lightseagreen" :weight bold))
        ("PRODUCT" . (:foreground "olivedrab" :weight bold))
        ("RELEASE" . (:foreground "maroon3" :weight bold))
        ("RESEARCH" . (:foreground "maroon2" :weight bold))
        ("HACK" . (:foreground "maroon3" :weight bold))
        ("TBD" . (:foreground "brown" :weight bold))
        ("CODE" . (:foreground "bisque" :weight bold :background "midnightblue"))
        ("HOLD" . (:foreground "red1" :weight bold :background "yellow1"))
        ("WAIT" . (:foreground "red4" :weight bold :background "yellow1"))
        ("WIP" . (:foreground "darkorchid2" :weight bold))
        ("NOPE" . (:foreground "hotpink" :weight bold :background "darkgreen"))))

;; link abbrevs
(setq org-link-abbrev-alist
      '(("vc" . "https://vc.compiler.company/%s")
        ("comp" . "https://compiler.company/%s")
	("cdn" . "https://cdn.compiler.company/%s")
        ("packy" . "https://packy.compiler.company/%s")
        ("yt" . "https://youtube.com/watch?v=%s")
        ("wikipedia" . "https://en.wikipedia.org/wiki/%s")
        ("reddit" . "https://reddit.com/%s")
        ("hn" . "https://news.ycombinator.com/%s")
        ("so" . "https://stackoverflow.com/%s")))

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

(defun org-id-add-to-headlines-in-files (&optional files)
  (interactive)
  (with-temp-buffer
    (dolist (f (or files org-agenda-files))
      (find-file f)
      (org-id-add-to-headlines-in-file)
      (save-buffer))))

(defun org-id-add-to-headlines-in-directory (&optional dir)
  (interactive)
  (let ((dir (or dir org-directory)))
    (org-id-add-to-headlines-in-files
     (directory-files-recursively dir "[.]org$"))))

(message "Initialized ULANG.")

(provide 'ulang)
;;; ulang.el ends here
