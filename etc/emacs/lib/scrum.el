;;; scrum.el --- Scrum-like Planning and Roadmaps in Org  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Richard Westhaver

;; Author: Richard Westhaver <richard.westhaver@gmail.com>
;; Keywords: maint

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

;; The point of this package is to enable an Emacs-native scrum
;; workflow. Many years ago I used to use the org-jira package and
;; mirror an external scrum/agile system (Jira).

;; Mind you, I wouldn't dare take a shot at Jira. As far as Products
;; go, when you need to work with hundreds of humans on software and
;; are given a short list you must choose from, it's often the best of
;; the worst.

;; The problem is however, that we don't need Products. What we need,
;; is a plan. How we achieve that end should be via the best and most
;; powerful tools possible.

;; In my opinion, Emacs Org Mode is the most powerful tool
;; available. It is not quite the best tool for the job, but this
;; isn't a problem because it is not a Product. We are given the
;; opportunity to make it the best tool possible, in the only way
;; possible - by doing it ourselves.

;; And yes, the aura of NIH syndrome may be strong here. Most of the
;; time you need to work with lots of folks who don't have the need or
;; patience to learn Org-mode. This package isn't for them. It's for
;; small groups of like-minded Lispers :).

;;;; Refs
;; scrum: https://www.scrum.org/resources/what-scrum-module

;; roadmap: https://compiler.company/plan/roadmap.html

;; tasks: https://compiler.company/plan/tasks

;;;; API

;; The API is still very much a WIP. Assume everything below to be
;; theoretical.

;; - task dependencies
;;   - refer to org-depend.el for implementation details
;;   - org-trigger-hook and org-blocker-hook
;;   - org-todo-state-tags-triggers

;; - dynamic blocks
;;   - scrumboard
;;   - burndown

;;; Code:
(require 'ulang)

(defgroup scrum nil
  "CC Scrum Framework.")

(defvar scrum-properties '("SPRINT" "EPIC" "RELEASE" "TASKID" "PROJECT" "COMMIT" "GOAL"))

(defvar scrum-tags '("demo" "mvp" "release" "major-release" "ua" "qa"))

(defun org-dblock-write:scrumboard ()
  "Generate a 'scrumboard'.")

;; TODO 2024-09-06: eplot
(defun org-dblock-write:burndown ()
  "Generate a 'burndown' chart in the current buffer.")

;;; Projects
;; defining 'project' machinery here because we don't have a better
;; place to put it. These functions are intended to map projects
;; from 'skel' and 'project.el' into our task-based org system.

;; Projects can contain many subprojects, which are identified by org
;; headings with a 'PROJECT' todo keyword. Projects and sub-projects
;; all have a 'VERSION' property assigned which can't be
;; inherited. The 'PROJECT' property itself can be inherited.

;; project-info
(defcustom org-project-info-order '(details status churn files html tasks log vc links)
  "Order in which sections of the 'project-info' dblock will appear."
  :type 'list
  :group 'scrum)

(defcustom org-lisp-system-info-order '(log packages dependencies dependents files tests symbols)
  "Order in which sections of the 'lisp-system-info' dblock will appear."
  :type 'list
  :group 'scrum)

(defmacro with-dblock-defaults (&rest body)
  `(let ((location (or (when-let* ((param (plist-get params :location)))
			(cl-coerce param 'string))
		      (org-entry-get (point) "LOCATION")
		      (when-let* ((kw (org-collect-keywords '("LOCATION"))))
			(cadar kw))
		      (project-root (project-current))))
	 (point (point)))
     ,@body))

(defun org-dblock-write:project-info (params)
  "Generate a project-info section.

The following keyword parameters can be passed to the info dynamic block:

:location Set or override the project location which is inferred by
          checking for a LOCATION property in the current tree, followed
          by the value of the `project-current' function.

:branch Set or override the project branch to display info for. Default
        branch name is 'default'.

:churn   when nil don't include the vc churn report.
:log     when nil don't include the vc log.
:status  when nil don't include vc status.
:details when nil don't include the project details section.
:vc      when non-nil include the vc files table.
:files   when non-nil include the local files table.
:html    when non-nil include the html files table.

:links when non-nil include the links list. The argument is passed
       via project-links(include=ARG)."
  (with-dblock-defaults
   (let ((html (when-let* ((val (plist-member params :html)))
		 (cadr val)))
         (vc (when-let* ((val (plist-member params :vc)))
               (cadr val)))
	 (links (when-let* ((val (plist-member params :links)))
	       (cadr val)))
	 (files (when-let* ((val (plist-member params :files)))
		  (cadr val)))
         (churn (if-let* ((val (plist-member params :churn)))
                    (cadr val)
                  t))
         (status (if-let* ((val (plist-member params :status)))
                     (cadr val)
                   t))
         (log (if-let* ((val (plist-member params :log)))
                  (cadr val)
		t))
         (tasks (if-let* ((val (plist-member params :tasks)))
                    (cadr val)
                  t))
         (details (if-let* ((val (plist-member params :details)))
                      (cadr val)
                    t)))
     (message "Generating info for project: %s" location)
     (let* ((project (project-current nil location))
            (project-name (project-name project))
            (project-root (project-root project)))
       (dolist (i org-project-info-order)
         (pcase i
           ('details (when details
                       (message "building project details...")
                       (insert "#+CALL: project-details(")
		       (unless vc (insert "vc='nil"))
		       (insert ") :dir " project-root "\n")
                       (org-babel-execute-maybe)
                       (org-table-align)))
           ('status (when status
                      (message "building project status...")
                      (insert "#+CALL: hg-diff-stat() :dir " project-root "\n")))
           ('tasks (when tasks
                     (message "building project tasks...")
                     (insert "#+CALL: project-tasks() :dir " project-root "\n")))
           ('churn (when churn
                     (message "building project vc churn...")
                     (insert "#+CALL: hg-churn() :dir " project-root "\n")))
           ('log (when log
                   (message "building project vc log...")))
	   ('html (when html
		    (message "building project html files...")
		    (insert "#+CALL: project-html-files() :dir " project-root "\n")))
           ('vc (when vc
                  (message "building project vc files...")
                  (insert "#+CALL: project-vc-files() :dir " project-root "\n")))
	   ('files (when files
		     (message "building project local files...")
		     (insert "#+CALL: project-files() :dir " project-root "\n")))
	   ('links (when links
		     (message "building project links...")
		     ;; note that LINKS is quoted
		     (insert "#+CALL: project-links(include=" 
			     (format "'%s" links) ") :dir " project-root "\n")))))
       (org-babel-execute-region point (point))))))

(defun org-project-info ()
  "Insert or update a project-info dblock."
  (interactive)
  (if (re-search-forward (rx bol "#+BEGIN:" (+ space) "project-info") nil t)
      (progn
        (if (fboundp 'org-fold-show-entry)
            (org-fold-show-entry)
          (with-no-warnings (org-show-entry)))
        (beginning-of-line))        
    (org-create-dblock (list :name "project-info")))
  (org-update-dblock))

(org-dynamic-block-define "project-info" 'org-project-info)

(defun org-dblock-write:lisp-system-info (params)
  "Generate a project-info section.

The following keyword parameters can be passed to the info dynamic
block:

:location Set or override the project location which is inferred by
       checking for a LOCATION property in the current tree, followed by
       the value of the `project-current' function.

:log          when nil don't include the vc log.
:files        when nil don't include the files section.
:packages     when nil don't include the packages section.
:symbols      when nil don't include the symbols section.
:tests        when nil don't include the tests section.
:dependencies when nil don't include the dependencies section.
:dependents   when nil don't include the dependents section.
:level        when non-nil insert sections as headings at the level indicated,
              else sections returned as lists."
  (with-dblock-defaults
   (let ((files (if-let* ((val (plist-member params :files)))
		    (cadr val)
		  t))
	 (system (if-let* ((val (plist-member params :system)))
		     (cadr val)
		   t))
	 (packages (if-let* ((val (plist-member params :packages)))
		       (cadr val)
		     t))
	 (symbols (if-let* ((val (plist-member params :symbols)))
		      (cadr val)
		    t))
	 (tests (if-let* ((val (plist-member params :tests)))
		    (cadr val)
		  t))
	 (dependencies (if-let* ((val (plist-member params :dependencies)))
			   (cadr val)
			 t))
	 (level (if-let* ((val (plist-member params :level)))
		    (cadr val)
		  nil))
	 (dependents (if-let* ((val (plist-member params :dependents)))
			 (cadr val)
		       t)))
     (message "Generating info for lisp-system: %s" location)
     (let* ((project (project-current nil location))
	    (project-name (project-name project))
	    (project-root (project-root project))
	    (section-prefix (if level (make-string level ?*) "-")))
       (dolist (i org-lisp-system-info-order)
	 (pcase i
	   ('dependents (when dependents
			  (message "building lisp-system dependents...")
			  (insert (format "%s dependents\n  #+CALL: lisp-system-dependents[:post transpose](\"%s\")" 
					  section-prefix
					  system)
				  "\n")))
	   ('dependencies (when dependencies
			  (message "building lisp-system dependencies...")
			  (insert (format "%s dependencies\n  #+CALL: lisp-system-dependencies[:post transpose](\"%s\")" 
					  section-prefix
					  system)
				  "\n")))
	   ('files (when files
		     (message "building lisp-system files...")
		     (insert (format "%s files\n  #+CALL: lisp-system-files[:post transpose](\"%s\")" 
				     section-prefix
				     system) 
			     "\n")))
	   ('packages (when packages
		     (message "building lisp-system packages...")
		     (insert (format "%s packages\n  #+CALL: lisp-system-packages[:post transpose](\"%s\")" 
				     section-prefix
				     system)
			     "\n")))
	   ('symbols (when symbols
		     (message "building lisp-system symbols...")
		     (insert (format "%s symbols\n  #+CALL: lisp-package-symbols[:post transpose](\"%s\")" 
				     section-prefix
				     (upcase (format "%s" system))) 
			     "\n")))
	   ('tests (when tests
		     (message "building lisp-system tests...")
		     (insert (format "%s tests\n  #+CALL: lisp-system-tests[:post transpose](\"%s\")" 
				     section-prefix
				     system) 
			     "\n")))))
       (org-babel-execute-region point (point))))))

(defun org-lisp-system-info (&optional system)
  "Insert or update a lisp-system-info dblock."
  (interactive "S")
  (if (re-search-forward (rx bol "#+BEGIN:" (+ space) "lisp-system-info") nil t)
      (progn
	(if (fboundp 'org-fold-show-entry)
	    (org-fold-show-entry)
	  (with-no-warnings (org-show-entry)))
	(beginning-of-line))
    (org-create-dblock (list :name "lisp-system-info" :system system)))
  (org-update-dblock))

(org-dynamic-block-define "lisp-system-info" 'org-lisp-system-info)

(provide 'scrum)
;;; scrum.el ends here
