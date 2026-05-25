;;; config.el --- Default User Configuration         -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The Compiler Company

;; Author: Richard Westhaver <richard.westhaver@gmail.com>
;; Keywords: 

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

;;; Org

;;;; Directories
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

;;;; Babel
;; org-sbx [[https://list.orgmode.org/d429d29b-42fa-7d7b-6f3a-9fe692fd6dc7@grinta.net/T/]]
(defun %org-sbx (name header args)
  (let* ((args (mapconcat
		(lambda (x)
		  (format "%s=%S" (symbol-name (car x)) (cadr x)))
		args ", "))
	 (ctx (list 'babel-call (list :call name
				      :name name
				      :inside-header header
				      :arguments args
				      :end-header ":results silent")))
	 (info (org-babel-lob-get-info ctx)))
    (when info (org-babel-execute-src-block nil info))))

(defmacro org-sbx (name &rest args)
  (let* ((header (if (stringp (car args)) (car args) nil))
	 (args (if (stringp (car args)) (cdr args) args)))
    (unless (stringp name)
      (setq name (symbol-name name)))
    (let ((result (%org-sbx name header args)))
      (org-trim (if (stringp result) result (format "%S" result))))))

(defun org-babel-execute-region (beg end &optional arg)
  (interactive "r")
  (narrow-to-region beg end)
  (org-babel-execute-buffer arg)
  (widen))

;;;; Agenda
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

;;;; Capture
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

;;;; Check
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

;;;; Links
; to include mm-url-decode-entities-string
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
