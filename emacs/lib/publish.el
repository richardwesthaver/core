;;; publish.el --- the.compiler.company publishing script  -*- lexical-binding: t; -*-

;; emacsclient -e '(load-file "publish.el")' '(publish)'

;;; Code:
(require 'ox-publish)
(require 'org-id)
;; vendored
(require 'htmlize)
(defvar project-dir "~/dev/comp/org")
(defvar publish-dir "/mnt/w/compiler.company")
(defvar html-theme nil)
(defvar url "https://compiler.company")
(defvar html-nav "<div class=\"nav\"> (<a href = \"https://compiler.company\">~</a> (<a href = \"https://compiler.company/blog\">blog</a> <a href = \"https://compiler.company/docs\">docs</a>) (<a href = \"https://vc.compiler.company/comp\">vc</a> <a href = \"https://packy.compiler.company\">packy</a>))</div>")

(defvar html-foot "<footer><p>updated %C</p></footer>")

(setq org-html-style-default ""
      org-html-scripts ""
      org-html-htmlize-output-type 'css
      org-export-htmlize-output-type 'css
      org-html-doctype "html5"
      org-html-html5-fancy t
      org-html-validation-link nil
      org-src-fontify-natively t
      make-backup-files nil
      debug-on-error t
      org-id-link-to-org-use-id t)

(setq org-publish-project-alist
      `(("compiler.company" :components ("index" "blog" "docs"))
        ("index"
         :base-directory ,project-dir
         :base-extension "org"
         :recursive nil
         :htmlized-source t
         :footnote-section-p t
         :html-doctype "<!doctype html>"
	 :html-preamble ,html-nav
	 :html-postamble ,html-foot
         :publishing-directory ,publish-dir
         :publishing-function org-html-publish-to-html)
        ("blog"
         :base-directory ,(expand-file-name "blog" project-dir)
         :base-extension "org"
         :footnote-section-p t
         :html-doctype "<!doctype html>"
	 :publishing-directory ,(expand-file-name "blog" publish-dir)
	 :publishing-function org-html-publish-to-html
	 :htmlized-source t
	 :html-preamble ,html-nav
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
         :html-preamble ,html-nav
	 :html-postamble ,html-foot)))

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
  "Add CUSTOM_ID properties to all headlines in the
   current file which do not already have one."
  (interactive)
  (org-map-entries (lambda () (org-custom-id-get (point) 'create))))

;;;###autoload
(defun publish (&optional sitemap static force async)
  "publish `rwest-io' content.
If STATIC is t, also publish media and static files.
If FORCE is t, skip checking file mod date and just publish all files.
If ASYNC is t, call `org-publish' asynchronously.
If given a prefix (C-u), set all args to t"
  (interactive)
  (if current-prefix-arg
      (setq static t
	    force t
            async t))
  (let ((default-directory project-dir))
    (message (format "publishing from %s" default-directory))    
    (org-publish "compiler.company" force async)))
