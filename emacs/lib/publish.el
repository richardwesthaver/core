;;; publish.el --- the.compiler.company publishing script  -*- lexical-binding:t -*-

;; emacsclient -e '(load-file "publish.el")' '(publish)'

;;; Code:
(require 'ox-publish)
(require 'org-id)
;; vendored
(require 'htmlize)
(defvar project-dir "~/comp/org")
(defvar publish-dir "/tmp/www")
(defvar html-theme nil)
(defvar url "https://compiler.company")
(defvar vc-url "https://vc.compiler.company")
(defvar packy-url "https://packy.compiler.company")
(defvar html-nav (format "<div class=\"nav\" id=\"nav\"><h2 id=\"index\">*</h2><div id=\"text-index\"> (<a href = \"%s\">~</a><br> (<a href = \"%s/blog\">blog</a> <a href = \"%s/docs\">docs</a> <a href = \"%s/plan\">plan</a> <a href = \"%s/notes\">notes</a>)<br> (<a href = \"%s\">vc</a> <a href = \"%s\">packy</a>))</div></div>"
                       url url url url url vc-url packy-url))

(defvar html-foot "<footer><p>updated %C</p></footer>")

;; (setq org-protocol-project-alist
;;       '(("comp"
;;          :base-url url
;;          :working-directory project-dir
;;          :online-suffix ".html"
;;          :working-suffix ".org")))

(setq org-html-style-default ""
      org-html-scripts ""
      org-html-htmlize-output-type 'css
      org-export-htmlize-output-type 'css
      org-export-allow-bind-keywords t
      org-html-doctype "html5"
      org-html-html5-fancy t
      org-html-validation-link nil
      org-src-fontify-natively t
      make-backup-files nil
      debug-on-error t
      org-id-link-to-org-use-id t)

(setq org-publish-project-alist
      `(("compiler.company" :components ("index" "meta" "blog" "docs" "notes" "plan"))
        ("index"
         :base-directory ,project-dir
         :base-extension "org"
         :recursive nil
         :htmlized-source t
         :footnote-section-p t
         :html-doctype "<!doctype html>"
	 ;; :html-preamble ,html-nav
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
         :html-preamble ,html-nav
         :html-postamble ,html-foot)
        ("blog"
         :base-directory ,(expand-file-name "blog" project-dir)
         :recursive t
         :base-extension "org"
         :footnote-section-p t
         :html-doctype "<!doctype html>"
	 :publishing-directory ,(expand-file-name "blog" publish-dir)
	 :publishing-function org-html-publish-to-html
	 :htmlized-source t
	 :html-preamble ,html-nav
	 :html-postamble ,html-foot)
        ("plan"
         :base-directory ,(expand-file-name "plan" project-dir)
         :recursive t
         :base-extension "org"
         :footnote-section-p t
         :html-doctype "<!doctype html>"
         :publishing-directory ,(expand-file-name "plan" publish-dir)
         :publishing-function org-html-publish-to-html
         :htmlized-source t
         :html-preamble ,html-nav
         :html-postamble ,html-foot)
        ("notes"
         :base-directory ,(expand-file-name "notes" project-dir)
         :recursive t
         :base-extension "org"
         :footnote-section-p t
         :html-doctype "<!doctype html>"
         :publishing-directory ,(expand-file-name "notes" publish-dir)
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
(provide 'publish)
;;; publish.el ends here
