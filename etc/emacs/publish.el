;;; publish.el --- the.compiler.company publishing script  -*- lexical-binding:t -*-

;; emacsclient -e '(load-file "publish.el")' '(publish)'

;;; Code:
(require 'ox-publish)
(require 'org-id)
(require 'dash)
(require 'ox-man)
(require 'htmlize)
(defvar project-dir company-org-directory)
(defvar publish-dir "/tmp/www")
(defvar url "https://compiler.company")
(defvar vc-url company-vc-url)
(defvar packy-url company-packy-url)
(defvar html-foot "<footer><p>updated %C</p></footer>")
(defvar default-org-export-setupfile (join-paths company-org-directory "clean.theme"))

(setq org-html-style-default ""
      org-html-scripts nil
      org-html-htmlize-output-type 'css
      org-export-htmlize-output-type 'css
      org-export-allow-bind-keywords t
      org-export-async-init-file (join-paths company-org-directory "init.el")
      org-export-with-clocks t
      org-export-with-date t
      org-export-with-drawers t
      org-export-async-debug t
      org-html--id-attr-prefix ""
      ;; org-export-in-background t
      org-html-divs '((preamble "section" "preamble") (content "main" "content") (postamble "section" "postamble"))
      org-html-container-element "div"
      org-html-viewport '((width "device-width") (initial-scale 1))
      org-html-doctype "html5"
      org-html-html5-fancy t
      org-src-fontify-natively t
      org-export-with-broken-links 'mark
      org-html-checkbox-type 'unicode
      org-html-mathjax-options
      '((path "https://cdn.compiler.company/js/tex-mml-chtml.js")
	(scale 1.0) (align "center") (font "mathjax-modern")
	(overflow "overflow") (tags "ams") (indent "0em")
	(multlinewidth "85%") (tagindent ".8em") (tagside "right"))
      ;; org-html-creator-string
      make-backup-files nil
      debug-on-error t
      org-id-link-to-org-use-id t
      ;; org-html-self-link-headlines t
      org-html-format-drawer-function 'org-html-format-drawer)

(defmacro with-org-publish (&rest body)
  `(let (
	 (save-silently t)
	 (debug-on-error t)
	 (coding-system-for-read 'utf-8-unix)
	 (coding-system-for-write 'utf-8-unix)
	 (org-inhibit-startup t)
	 (org-mode-hook nil)
	 (find-file-hook nil)
	 (kill-buffer-hook nil)
	 (org-element-use-cache nil)
	 (before-save-hook nil)
	 (after-save-hook nil)
	 (kill-buffer-query-functions nil)
	 (buffer-list-update-hook nil))
     ,@body))

(setq org-html-home/up-format 
      "<nav id=\"org-div-home-and-up\">
<a href=\"%s\" accesskey=\"u\"><button class=up>↩</button></a>
<a href=\"%s\" accesskey=\"h\"><button class=home>⌂</button></a>
<button accesskey=\"s\" class=show onclick=open_all_sections()>show</button>
<button accesskey=\"x\" class=hide onclick=close_all_sections()>hide</button></nav>")
      
(setq org-publish-project-alist
      `(("compiler.company" :components ("index" "meta" "docs" "graph" "plan" "notes" "archive"))
        ("index"
         :base-directory ,project-dir
         :base-extension "org"
         :recursive nil
         :htmlized-source t
         :footnote-section-p t
         :html-doctype "<!doctype html>"
	 :html-postamble ,html-foot
         :publishing-directory ,publish-dir
         :publishing-function org-html-publish-to-html)
        ("meta"
         :base-directory ,(expand-file-name "meta" project-dir)
         :base-extension "org"
         :recursive t
         :footnote-section-p t
         :html-doctype "<!doctype html>"
         :publishing-directory ,(expand-file-name "meta" publish-dir)
         :publishing-function org-html-publish-to-html
         :htmlized-source t
         :html-postamble ,html-foot)
	("notes"
	 :base-directory ,(expand-file-name "notes" project-dir)
	 :base-extension "org"
	 :recursive t
	 :footnote-section-p t
	 :html-doctype "<!doctype html>"
	 :publishing-directory ,(expand-file-name "notes" publish-dir)
	 :publishing-function org-html-publish-to-html
	 :htmlized-source t
	 :html-postamble ,html-foot)
	("archive"
	 :base-directory ,(expand-file-name "archive" project-dir)
	 :base-extension "org"
	 :recursive t
	 :footnote-section-p t
	 :html-doctype "<!doctype html>"
	 :publishing-directory ,(expand-file-name "archive" publish-dir)
	 :publishing-function org-html-publish-to-html
	 :htmlized-source t
	 :html-postamble ,html-foot)
        ("plan"
         :base-directory ,(expand-file-name "plan" project-dir)
         :recursive t
         :with-todo-keywords t
         :with-properties t
         :base-extension "org"
         :footnote-section-p t
         :html-doctype "<!doctype html>"
         :publishing-directory ,(expand-file-name "plan" publish-dir)
         :publishing-function org-html-publish-to-html
         :htmlized-source t
         :html-postamble ,html-foot)
        ("graph"
         :base-directory ,(expand-file-name "graph" project-dir)
         :recursive t
         :base-extension "org"
         :with-properties t
         :with-drawers t
         :footnote-section-p t
         :html-doctype "<!doctype html>"
         :publishing-directory ,(expand-file-name "graph" publish-dir)
         :publishing-function org-html-publish-to-html
         :htmlized-source t
         :html-postamble ,html-foot)
        ("docs"
         :base-directory ,(expand-file-name "docs" project-dir)
         :base-extension "org"
         :recursive t
         :footnote-section-p t
         :html-doctype "<!doctype html>"
         :publishing-directory ,(expand-file-name "docs" publish-dir)
         :publishing-function org-html-publish-to-html
         :htmlized-source t
         :html-postamble ,html-foot)))

;;;###autoload
(defun update-sitemap ()
  "Update compiler.company sitemaps."
  (interactive)
  (save-excursion
    (let ((dirs '("graph/app" "graph/comp" "graph/lang" "graph/hw" "graph/math" "graph/os" 
		  "graph/proto" "graph/sys" "graph/theory" "graph/web"
		  "plan/tasks"
		  "docs/core/lib")))
      (while dirs
	(let* ((dir (pop dirs))
	       (default-directory (join-paths project-dir dir))
	       (files (remove "index.org" (directory-files default-directory nil ".org$" t)))
	       (entries))
	  (delete-file "index.org")
	  (when-let* ((index-open (find-buffer-visiting "index.org")))
	    (kill-buffer index-open))
	  (while files
	    (let* ((file (pop files)))
	      (with-temp-buffer
		(org-mode)
		(insert-file-contents file nil)
		(add-to-list
		 'entries
		 (cons file (org-collect-keywords '("TITLE")))))))
	  (sort entries
		(lambda (a b)
		  (string-greaterp (car (cdaddr a)) (car (cdaddr b)))))
	  (org-with-file-buffer "index.org"
	    (insert (format "#+TITLE: %s\n" dir))
	    (insert (format "#+HTML_LINK_UP: %s\n" "../"))
	    (insert (format "#+SETUPFILE: %s\n" 
			    (cl-case (length (file-name-split dir))
			      (2 "../../clean.theme")
			      (3 "../../../clean.theme")
			      (4 "../../../../clean.theme")
			      (t (error "max index level reached in 'update-sitemap'")))))
	    (dolist (e entries)
	      (let ((file (file-name-with-extension (car e) "html"))
		    (title (cadadr e)))
		(insert (format "- [[%s/%s/%s][%s]]\n" url dir file title))))
	    (save-buffer))
	  (message (format "generated %s/index.org" dir)))))))

;;;###autoload
(defun publish (&optional sitemap force async)
  "publish `compiler.company' content.
If SITEMAP is t, generate sitemaps.
If FORCE is t, skip checking file mod date and just publish all files.
If ASYNC is t, call `org-publish' asynchronously.
If given a prefix (C-u), set all args to t"
  (interactive)
  (with-org-publish
   (when current-prefix-arg
     (setq sitemap t
	   force t
	   async t))
   (let ((default-directory project-dir))
     (message (format "publishing %s" default-directory))
     (when sitemap (update-sitemap))
     (org-publish "compiler.company" force async)
     publish-dir)))

(defun publish* (&optional force)
  "start async compiler.company publish."
  (interactive)
  (when current-prefix-arg (setq force t))
  (org-publish-project "index" force t)
  (org-publish-project "meta" force t)
  (update-sitemap)
  (dolist (p '("plan" "docs" "graph" "notes"))
    (org-publish-project p force t))
  (org-export-stack))

(org-export-define-derived-backend 'cc-html 'html
  :menu-entry
  '(?c "Export CC HTML"
       ((?C "As HTML buffer" org-cc-html-export-as-html)
	(?c "As HTML buffer" org-cc-html-export-to-html)
	(?o "As HTML file and open"
	    (lambda (a s v b)
	      (if a (org-cc-html-export-to-html t s v b)
		(org-open-file (org-cc-html-export-to-html nil s v b))))))))

(defun org-cc-html-edge-drawer (drawer contents info)
  (funcall (plist-get info :html-format-drawer-function)
	   (org-element-property :drawer-name drawer)
	   contents))

(provide 'publish)
;;; publish.el ends here
