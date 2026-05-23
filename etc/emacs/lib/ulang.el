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
(require 'org-element)
(require 'ox)
(require 'ol-man)
(require 'ol-info)
(defgroup ulang nil
  "CC Universal Language.")

(defvar ulang-link-history nil)
(defvar ulang-file-history nil)
;; 
(defvar ulang-special-properties
  "See 'org-special-properties'."
  '("VERSION"))

(defvar ulang-info-url-alist
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
    ("guile" . "https://www.gnu.org/software/guile/manual/html_node/"))
  "See 'org-info-other-documents'.")

(setq org-man-command 'woman)

(org-export-translate-to-lang (list '("Table of Contents" "⇜")) "ulang")

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

(defun org-clock-in-wip ()
  "Clock in when todo state is changed to WIP."
  (when (string= (org-get-todo-state) "WIP")
    (unless (org-clocking-buffer)
      (org-clock-in))))

(add-hook 'org-after-todo-state-change-hook #'org-clock-in-wip)

;; link abbrevs
(require 'ol-irc)
(defun ol-vc-expand (tag)
  "Expand the tag of an org-link where linkkey is `vc'."
  (let ((f (split-string tag ":" "/")))
    (concat (string-trim-right company-vc-url "[/]")
	    (cl-case (length f)
	      (0 "")
	      (1 (format "/%s" (car f)))
	      (2 (apply 'format "/%s/file/tip/%s" f))
	      (t (apply 'format "/%s/file/%s/%s" f))))))

(setq org-link-abbrev-alist
      `(("vc" . ol-vc-expand)
        ("comp" . ,(format "https://%s/%%s" company-domain))
	("cdn" . ,(format "%s/%%s" company-cdn-url))
        ("packy" . ,(format "%s/%%s" packy-url))
        ("yt" . "https://youtube.com/watch?v=%s")
	("gh" . "https://github.com/%s")
	("cb" . "https://codeberg.org/%s")
	("wikipedia" . "https://en.wikipedia.org/wiki/%s")
	("archwiki" . "https://wiki.archlinux.org/title/%s")
        ("reddit" . "https://reddit.com/%s")
        ("hn" . "https://news.ycombinator.com/%s")
	("archive" . "https://web.archive.org/web/%s")
        ("so" . "https://stackoverflow.com/%s")))

;;; IDs
(defun org-title-to-filename (title)
  "Convert TITLE to a reasonable filename."
  ;; Based on the slug logic in org-roam, but org-roam also uses a
  ;; timestamp.
  (setq title (downcase title))
  (setq title (s-replace-regexp "[^a-zA-Z0-9]+" "-" title))
  (setq title (s-replace-regexp "-+" "-" title))
  (setq title (s-replace-regexp "^-" "" title))
  (setq title (s-replace-regexp "-$" "" title))
  title)

(defun org-get-custom-id-list ()
  (flatten
   (org-map-entries
    (lambda ()
      (org-entry-get nil "CUSTOM_ID")))))

(defun org-generate-custom-id (&optional id-list)
  (let* ((custom-id (org-entry-get nil "CUSTOM_ID"))
         (heading (org-heading-components))
         (level (nth 0 heading))            
         (todo (nth 2 heading))                       
         (headline (nth 4 heading))
         (slug (org-title-to-filename headline))
         (duplicate-id (when id-list (member slug id-list))))
    (when (not duplicate-id)
      (message "Adding CUSTOM_ID %s to %s" slug headline)
      (org-entry-put nil "CUSTOM_ID" slug))))

(defun org-generate-custom-ids ()                              
  "Generate CUSTOM_ID for any headings that are missing one"   
    (save-excursion                                            
      (org-with-wide-buffer                                    
       (let ((existing-ids (org-get-custom-id-list)))          
         (org-map-entries                                      
          (lambda ()                                           
            (org-generate-custom-id existing-ids)))))))

;;;###autoload
(defun org-id-add-to-headlines-in-file ()
  "Add ID properties to all headlines in the
   current file which do not already have one."
  (interactive)
  (org-map-entries (lambda () (org-id-get (point) 'create))))

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

(defun ulang-init ()
  (interactive)
  (org-babel-lob-ingest company-babel-file)
  (let ((%eq (lambda (a b) (equal (car a) (car b)))))
    (mapcar (lambda (x) 
	      (cl-pushnew x org-info-other-documents :test %eq)
	      (cl-pushnew x browse-url-filename-alist :test %eq))
	    ulang-info-url-alist)
    (message "Initialized ULANG.")))

;;; Commands

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
    (when (and (< 1 (length path)) (file-name-absolute-p (print (cadr path))))
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
          (beginning-of-buffer)
          (let ((start (point)))
            (when (re-search-forward (rx bol "#+LOCATION:" (+ space) (group (* (not space))) eol) nil t)
              (setq start (match-beginning 0))
              (goto-char start)
              (delete-line))
            (insert "#+LOCATION: " val "\n")))
      (org-set-property "LOCATION" value))))

(defun org-follow-location (point)
  "Open the location specified by the LOCATION property of the org heading
or file at point."
  (interactive "d")
  (let ((loc (org-get-location point)))
    (cond
     ((string-match-p org-link-any-re loc) (org-link-open-from-string loc))
     ;; TODO 2024-08-29: handle other location types (physical, etc)
     (t (find-file loc t)))))

(provide 'ulang)
;;; ulang.el ends here
