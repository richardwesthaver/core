;;; organ.el --- Org Extensions -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The Compiler Company

;; Author:  <ellis@zor>
;; Keywords: docs, outlines

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

;; Handful of org-specific extensions

;;; Code:
(require 'org)

;;; Directories
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

;;; Babel
(defun org-babel-execute-region (beg end &optional arg)
  (interactive "r")
  (narrow-to-region beg end)
  (org-babel-execute-buffer arg)
  (widen))

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

;;; Agenda
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

(defun org-todo-at-date (date)
  "create a todo entry for a given date."
  (interactive (list (org-time-string-to-time (org-read-date))))
  (cl-flet ((org-current-effective-time (&rest r) date)
	    (org-today (&rest r) (time-to-days date)))
    (cond ((eq major-mode 'org-mode) (org-todo))
	  ((eq major-mode 'org-agenda-mode) (org-agenda-todo)))))

(defun org-agenda-show-week-all (&optional arg) 
  (interactive "P") 
  (org-agenda arg "n"))

;;; Capture
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
(defun org-clock-in-wip ()
  "Clock in when todo state is changed to WIP."
  (when (string= (org-get-todo-state) "WIP")
    (unless (org-clocking-buffer)
      (org-clock-in))))
;;; Archive
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

;;; Check
(defun org-adjust-tags-column-reset-tags ()
  "In org-mode buffers, reset tag position according to `org-tags-column'."
  (interactive)
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

;;; Links
(use-package mm-url
  :autoload (mm-url-decode-entities-string))

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

;;; Export
(defun org-html-format-drawer (name contents)
  "Default function used as value for `org-html-format-drawer-function'."
  (let ((name (downcase name)))
    (format "<details class='edges'><summary>%s</summary>%s</details>"
	    name
	    (pcase name
	      ("edges"
	       (unless (null contents)
		 (let ((es (intersperse "<br>" (s-lines contents))))
		   (if (> (length es) 3)
		       (progn
			 (setf (cadr es) nil
			       (nth (1- (length es)) es) nil)
			 (apply 'concat (flatten es)))
		     (apply 'concat es)))))
	      (_ contents)))))

;; replace hardcoded value
(defun org-html-property-drawer (_drawer contents _info)
  "Transcode a PROPERTY-DRAWER element from Org to HTML.
CONTENTS holds the contents of the drawer.  INFO is a plist holding
contextual information."
  (format "<details class='properties'><summary>props</summary>\n%s</details>" (apply 'concat (intersperse "<br>" (s-lines contents)))))

(defun org-export-get-reference-title (datum info)
  "Like `org-export-get-reference', except uses heading titles instead of random numbers."
  (let ((cache (plist-get info :internal-references)))
    (or (car (rassq datum cache))
	(let* ((crossrefs (plist-get info :crossrefs))
	       (cells (org-export-search-cells datum))
	       ;; Preserve any pre-existing association between
	       ;; a search cell and a reference, i.e., when some
	       ;; previously published document referenced a location
	       ;; within current file (see
	       ;; `org-publish-resolve-external-link').

	       ;; However, there is no guarantee that search cells are
	       ;; unique, e.g., there might be duplicate custom ID or
	       ;; two headings with the same title in the file.

	       ;; As a consequence, before re-using any reference to
	       ;; an element or object, we check that it doesn't refer
	       ;; to a previous element or object.
	       (new (or (cl-some
			 (lambda (cell)
			   (let ((stored (cdr (assoc cell crossrefs))))
			     (when stored
			       (let ((old (org-export-format-reference stored)))
				 (and (not (assoc old cache)) stored)))))
			 cells)
			(when (org-element-property :raw-value datum)
			  ;; Heading with a title
			  (org-export-new-title-reference datum cache))
			;; NOTE: This probably breaks some Org Export
			;; feature, but if it does what I need, fine.
			(org-export-format-reference
			 (org-export-new-reference cache))))
	       (reference-string new))
	  ;; Cache contains both data already associated to
	  ;; a reference and in-use internal references, so as to make
	  ;; unique references.
	  (dolist (cell cells) (push (cons cell new) cache))
	  ;; Retain a direct association between reference string and
	  ;; DATUM since (1) not every object or element can be given
	  ;; a search cell (2) it permits quick lookup.
	  (push (cons reference-string datum) cache)
	  (plist-put info :internal-references cache)
	  reference-string))))

(defun org-export-new-title-reference (datum cache)
  "Return new reference for DATUM that is unique in CACHE."
  (cl-macrolet ((inc-suffixf (place)
		  `(progn
		     (string-match (rx bos
				       (minimal-match (group (1+ anything)))
				       (optional "--" (group (1+ digit)))
				       eos)
				   ,place)
		     ;; HACK: `s1' instead of a gensym.
		     (-let* (((s1 suffix) (list (match-string 1 ,place)
						(match-string 2 ,place)))
			     (suffix (if suffix
					 (string-to-number suffix)
				       0)))
		       (setf ,place (format "%s--%s" s1 (cl-incf suffix)))))))
    (let* ((title (org-element-property :raw-value datum))
	   (ref (url-hexify-string (substring-no-properties title)))
	   (parent (org-element-property :parent datum)))
      (while (--any (equal ref (car it))
		    cache)
	;; Title not unique: make it so.
	(if parent
	    ;; Append ancestor title.
	    (setf title (concat (org-element-property :raw-value parent)
				"--" title)
		  ref (url-hexify-string (substring-no-properties title))
		  parent (org-element-property :parent parent))
	  ;; No more ancestors: add and increment a number.
	  (inc-suffixf ref)))
      ref)))

(defun org-html--reference (datum info &optional named-only)
  "Return an appropriate reference for DATUM.
DATUM is an element or a `target' type object.  INFO is the
current export state, as a plist.
When NAMED-ONLY is non-nil and DATUM has no NAME keyword, return
nil.  This doesn't apply to headlines, inline tasks, radio
targets and targets."
  (let* ((type (org-element-type datum))
	 (user-label
	  (org-element-property
	   (pcase type
	     ((or `headline `inlinetask) :CUSTOM_ID)
	     ((or `radio-target `target) :value)
	     (_ :name))
	   datum))
	 (user-label (or user-label
			 (when-let* ((path (org-element-property :ID datum)))
			   path))))
    (cond
     ((and user-label
	   (or (plist-get info :html-prefer-user-labels)
	       ;; Used CUSTOM_ID property unconditionally.
	       (memq type '(headline inlinetask))))
      user-label)
     ((and named-only
	   (not (memq type '(headline inlinetask radio-target target)))
	   (not user-label))
      nil)
     (t
      (org-export-get-reference datum info)))))

(define-minor-mode org-id-export-mode
  "Attempt to export Org as HTML with useful link IDs.
Instead of random IDs like \"#orga1b2c3\", use heading titles, made
unique when necessary."
  :global t
  (if org-id-export-mode
      (advice-add #'org-export-get-reference :override #'org-export-get-reference)
    (advice-remove #'org-export-get-reference #'org-export-get-reference)))

(provide 'organ)
;;; organ.el ends here
