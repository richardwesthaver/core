;;; publish.el --- the.compiler.company publishing script  -*- lexical-binding:t -*-

;; emacsclient -e '(load-file "publish.el")' '(publish)'

;;; Code:
(require 'ox-publish)
(require 'org-id)
(require 'dash)
(require 'ox-man)
;; vendored
(require 'htmlize)
(defvar project-dir "~/comp/org")
(defvar publish-dir "/tmp/www")
(defvar url "https://compiler.company")
(defvar vc-url "https://vc.compiler.company")
(defvar packy-url "https://packy.compiler.company")
(defvar html-foot "<footer><p>updated %C</p></footer>")
(defvar default-org-export-setupfile (join-paths company-org-directory "clean.theme"))
;; (setq org-protocol-project-alist
;;       '(("comp"
;;          :base-url url
;;          :working-directory project-dir
;;          :online-suffix ".html"
;;          :working-suffix ".org")))

(setq org-html-style-default ""
      ;; org-html-scripts ""
      org-html-htmlize-output-type 'css
      org-export-htmlize-output-type 'css
      org-export-allow-bind-keywords t
      org-export-async-init-file (join-paths user-emacs-lib-directory "publish-init.el")
      org-export-async-debug t
      org-export-in-background t
      org-html-doctype "html5"
      org-html-html5-fancy t
      org-src-fontify-natively t
      org-export-with-broken-links 'mark
      make-backup-files nil
      debug-on-error t
      org-id-link-to-org-use-id t)

;; (setq org-html-klipsify-src t
;;       org-html-klipse-js (join-paths company-cdn-url "js/klipse.min.js")
;;       org-html-klipse-css (join-paths company-cdn-url "css/klipse.css")
;;       org-html-klipse-selection-script
;;       "window.klipse_settings = {selector_eval_html: '.src-html',
;; 			     selector_eval_js: '.src-js',
;; 			     selector_sql: '.src-sql',
;; 			     selector_pyodide: '.src-python',
;; 			     selector_eval_clisp: '.src-lisp',
;; 			     selector_eval_scheme: '.src-scheme',
;; 			     selector: '.src-clojure',
;; 			     selector_eval_ruby: '.src-ruby'};")

;; (setq org-html-link-home url)

(defmacro with-org-publish (&rest body)
  `(let (
	 ;; (save-silently t)
	 (debug-on-error t)
	 ;; (coding-system-for-read 'utf-8-unix)
	 ;; (coding-system-for-write 'utf-8-unix)
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

(setq org-html-home/up-format "<nav id=\"org-div-home-and-up\"><a href=\"%s\" accesskey=\"u\"><button class=up>▲</button></a><a href=\"%s\" accesskey=\"h\"><button class=home>⌂</button></a>
<button accesskey=\"s\" class=show onclick=open_all_sections()>show</button> <button accesskey=\"x\" class=hide onclick=close_all_sections()>hide</button></nav>")
      
(setq org-publish-project-alist
      `(("compiler.company" :components ("index" "meta" "blog" "docs" "graph" "plan"))
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
        ("blog"
         :base-directory ,(expand-file-name "blog" project-dir)
         :recursive t
         :base-extension "org"
         :footnote-section-p t
         :html-doctype "<!doctype html>"
	 :publishing-directory ,(expand-file-name "blog" publish-dir)
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

;; (defun org-export-get-reference-title (datum info)
;;   "Like `org-export-get-reference', except uses heading titles instead of random numbers."
;;   (let ((cache (plist-get info :internal-references)))
;;     (or (car (rassq datum cache))
;;         (let* ((crossrefs (plist-get info :crossrefs))
;;                (cells (org-export-search-cells datum))
;;                ;; Preserve any pre-existing association between
;;                ;; a search cell and a reference, i.e., when some
;;                ;; previously published document referenced a location
;;                ;; within current file (see
;;                ;; `org-publish-resolve-external-link').
;;                ;;
;;                ;; However, there is no guarantee that search cells are
;;                ;; unique, e.g., there might be duplicate custom ID or
;;                ;; two headings with the same title in the file.
;;                ;;
;;                ;; As a consequence, before re-using any reference to
;;                ;; an element or object, we check that it doesn't refer
;;                ;; to a previous element or object.
;;                (new (or (cl-some
;;                          (lambda (cell)
;;                            (let ((stored (cdr (assoc cell crossrefs))))
;;                              (when stored
;;                                (let ((old (org-export-format-reference stored)))
;;                                  (and (not (assoc old cache)) stored)))))
;;                          cells)
;;                         (when (org-element-property :raw-value datum)
;;                           ;; Heading with a title
;;                           (org-export-new-title-reference datum cache))
;;                         ;; NOTE: This probably breaks some Org Export
;;                         ;; feature, but if it does what I need, fine.
;;                         (org-export-format-reference
;;                          (org-export-new-reference cache))))
;;                (reference-string new))
;;           ;; Cache contains both data already associated to
;;           ;; a reference and in-use internal references, so as to make
;;           ;; unique references.
;;           (dolist (cell cells) (push (cons cell new) cache))
;;           ;; Retain a direct association between reference string and
;;           ;; DATUM since (1) not every object or element can be given
;;           ;; a search cell (2) it permits quick lookup.
;;           (push (cons reference-string datum) cache)
;;           (plist-put info :internal-references cache)
;;           reference-string))))

;; (defun org-export-new-title-reference (datum cache)
;;   "Return new reference for DATUM that is unique in CACHE."
;;   (cl-macrolet ((inc-suffixf (place)
;;                              `(progn
;;                                 (string-match (rx bos
;;                                                   (minimal-match (group (1+ anything)))
;;                                                   (optional "--" (group (1+ digit)))
;;                                                   eos)
;;                                               ,place)
;;                                 ;; HACK: `s1' instead of a gensym.
;;                                 (-let* (((s1 suffix) (list (match-string 1 ,place)
;;                                                            (match-string 2 ,place)))
;;                                         (suffix (if suffix
;;                                                     (string-to-number suffix)
;;                                                   0)))
;;                                   (setf ,place (format "%s--%s" s1 (cl-incf suffix)))))))
;;     (let* ((title (org-element-property :raw-value datum))
;;            (ref (url-hexify-string (substring-no-properties title)))
;;            (parent (org-element-property :parent datum)))
;;       (while (--any (equal ref (car it))
;;                     cache)
;;         ;; Title not unique: make it so.
;;         (if parent
;;             ;; Append ancestor title.
;;             (setf title (concat (org-element-property :raw-value parent)
;;                                 "--" title)
;;                   ref (url-hexify-string (substring-no-properties title))
;;                   parent (org-element-property :parent parent))
;;           ;; No more ancestors: add and increment a number.
;;           (inc-suffixf ref)))
;;       ref)))

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
			 (when-let ((path (org-element-property :ID datum)))
			   (concat "ID-" path)))))
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

;;;###autoload
(defun update-sitemap ()
  "update `rwest-io' sitemaps:
- blog
- notes
- projects"
  (interactive)
  (save-excursion
    (let ((dirs '("blog/draft" 
		  "graph/app" "graph/comp" "graph/lang" "graph/hw" "graph/math" "graph/os" 
		  "graph/proto" "graph/sys" "graph/theory" "graph/web"
		  "plan/tasks"
		  "docs/core/app" "docs/core/lib")))
      (message (format "generating sitemaps: %s" dirs))
      (while dirs
	(let* ((dir (pop dirs))
	       (default-directory (join-paths project-dir dir))
	       (files (remove "index.org" (directory-files default-directory nil ".org$" t)))
	       (entries))
	  (delete-file "index.org")
	  (when-let ((index-open (find-buffer-visiting "index.org")))
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

(org-export-define-derived-backend 'cc-html 'html
  :menu-entry
  '(?c "Export compiler.company HTML"
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

(defun org-cc-html-drawer (drawer contents info)
  (org-html-drawer drawer contents info))

(defun org-cc-html-format-drawer (name contents)
  contents)

(provide 'publish)
;;; publish.el ends here
