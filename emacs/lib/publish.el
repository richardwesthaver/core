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
(defvar html-theme nil)
(defvar url "https://compiler.company")
(defvar vc-url "https://vc.compiler.company")
(defvar packy-url "https://packy.compiler.company")
(defvar html-foot "<footer><p>updated %C</p></footer>")

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
      org-html-doctype "html5"
      org-html-html5-fancy t
      ;; org-html-validation-link nil
      org-src-fontify-natively t
      make-backup-files nil
      debug-on-error t
      org-id-link-to-org-use-id t)

(setq org-html-link-up "")
(setq org-html-link-home url)

(setq org-html-home/up-format "<div id=\"org-div-home-and-up\"><a href=\"%s\" accesskey=\"h\"><button class=home>~</button></a>
<button accesskey=\"s\" class=show onclick=open_all_sections()>show</button> <button accesskey=\"x\" class=hide onclick=close_all_sections()>hide</button></div>")
      
(setq org-publish-project-alist
      `(("compiler.company" :components ("index" "meta" "blog" "docs" "notes" "plan"))
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
         :recursive nil
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
         :base-extension "org"
         :footnote-section-p t
         :html-doctype "<!doctype html>"
         :publishing-directory ,(expand-file-name "plan" publish-dir)
         :publishing-function org-html-publish-to-html
         :htmlized-source t
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
