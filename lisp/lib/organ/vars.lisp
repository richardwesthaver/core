(in-package :organ)

(defparameter *org-todo-keyword-types*
  '(todo wip done))

(defparameter *org-todo-keywords*
  '(("TODO" . todo) ("DONE" . done) ("FIND" . todo) ("FOUND" . done)
    ("RESEARCH" . todo) ("RECORD" . todo) ("OUTLINE" . todo) ("DRAFT" . todo)
    ("REVIEW" . todo) ("FIX" . todo) ("IMPL" . todo) ("TEST" . todo) ("FIXED" . done)
    ("GOTO" . todo) ("HACK" . todo) ("NOTE" . todo) ("CODE" . todo) ("LINK" . todo))
  "List of keywords accepted by `organ'. ")

(defun org-todo-keyword-map ()
  (let ((kws (make-hash-table :size 20 :test #'equal)))
    (loop for (k . v) in *org-todo-keywords*
          do (setf (gethash k kws) v))
    kws))

(defvar org-todo-keyword-map (org-todo-keyword-map))

(defun org-todo-keyword-p (kw)
  "Search for symbol KW in `org-todo-keyword-map' returning the
associated value or nil if not found."
  (gethash kw org-todo-keyword-map nil))

(defvar org-headline-rx (create-scanner "^([*]+)\\s+(.*)$"))
(defvar org-todo-keyword-rx (create-scanner "^(\\w+)\\s+(.*)$"))
(defvar org-file-property-rx (create-scanner "^[#+](.*)[:]\\s+(.*)$"))
(defvar org-property-rx (create-scanner "^[:](.*)[:]\\s+(.*)$"))
(defvar org-priority-rx (create-scanner "^\\s*\\[#([A-Z0-9]+)\\]"))
(defvar org-property-start-rx (create-scanner "^[:]PROPERTIES[:]\\s*$"))
(defvar org-logbook-start-rx (create-scanner "^[:]LOGBOOK[:]\\s*$"))
(defvar org-end-rx (create-scanner "^[:]END[:]\\s*$"))
;; includes whitespace (need to trim-right)
(defvar org-planning-rx (create-scanner "^(SCHEDULED|DEADLINE|CLOSED)[:]\\s+(.*)\\s*$"))
(defvar org-scheduled-rx (create-scanner "\\<SCHEDULED:"))
(defvar org-deadline-rx (create-scanner "\\<DEADLINE:"))
(defvar org-src-block-rx (create-scanner "^\\([ 	]*\\)#\\+begin_src[ 	]+\\([^ 	
]+\\)[ 	]*\\([^\":
]*\"[^\"
*]*\"[^\":
]*\\|[^\":
]*\\)\\([^
]*\\)
\\([^ ]*?
\\)??[ 	]*#\\+end_src"))

;; this doesn't consume leading whitespace. It could be useful in the
;; future to infer a value for org-tags-column but is contained in the
;; title slot of `org-headline' for now. The result of this scan is a
;; single string delimited by the ':' character. To get a list of tags
;; as strings, use `org-tag-split'.
(defvar org-tag-rx (create-scanner "(:[\\w_@#%:]+:)$"))

;; TODO 2023-12-21: for some reason CL-PPCRE:SPLIT isn't working as expected here.
(defun org-tag-split (tags)
  (loop for tag in (uiop:split-string tags :separator '(#\: #\Space #\Tab))
        unless (sequence:emptyp tag)
          collect tag))

(defvar org-object-rx 
  (create-scanner "\\(?:[_^][-{(*+.,[:alnum:]]\\)\\|[*+/=_~][^[:space:]]\\|\\<\\(?:b\\(?:bdb\\|ibtex\\)\\|d\\(?:enote\\|o\\(?:cview\\|i\\)\\)\\|e\\(?:l\\(?:feed\\|isp\\)\\|ww\\)\\|f\\(?:ile\\(?:\\+\\(?:\\(?:emac\\|sy\\)s\\)\\)?\\|tp\\)\\|gnus\\|h\\(?:elp\\|ttps?\\)\\|i\\(?:d\\|nfo\\|rc\\)\\|m\\(?:ai\\(?:lto\\|rix\\)\\|he\\)\\|n\\(?:ews\\|otmuch\\(?:-\\(?:search\\|tree\\)\\)?\\)\\|rmail\\|shell\\|w3m\\):\\|\\[\\(?:cite[:/]\\|fn:\\|\\(?:[0-9]\\|\\(?:%\\|/[0-9]*\\)\\]\\)\\|\\[\\)\\|@@\\|{{{\\|<\\(?:%%\\|<\\|[0-9]\\|\\(?:b\\(?:bdb\\|ibtex\\)\\|d\\(?:enote\\|o\\(?:cview\\|i\\)\\)\\|e\\(?:l\\(?:feed\\|isp\\)\\|ww\\)\\|f\\(?:ile\\(?:\\+\\(?:\\(?:emac\\|sy\\)s\\)\\)?\\|tp\\)\\|gnus\\|h\\(?:elp\\|ttps?\\)\\|i\\(?:d\\|nfo\\|rc\\)\\|m\\(?:ai\\(?:lto\\|rix\\)\\|he\\)\\|n\\(?:ews\\|otmuch\\(?:-\\(?:search\\|tree\\)\\)?\\)\\|rmail\\|shell\\|w3m\\)\\)\\|\\$\\|\\\\\\(?:[a-zA-Z[(]\\|\\\\[ 	]*$\\|_ +\\)\\|\\(?:call\\|src\\)_"))

(defvar org-timestamp-rx
  (create-scanner "[[<]\\([[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}\\(?: .*?\\)?\\)[]>]\\|\\(?:<[0-9]+-[0-9]+-[0-9]+[^>
]+?\\+[0-9]+[dwmy]>\\)\\|\\(?:<%%\\(?:([^>
]+)\\)>\\)"))

(defvar org-ts-rx
  (create-scanner "<\\([[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}\\(?: .*?\\)?\\)>")
  "fast matching for timestamps")

(defvar org-table-any-line-rx (create-scanner "^[ \t]*\\(|\\|\\+-[-+]\\)"))

(defvar org-table-any-border-rx (create-scanner "^[ \t]*[^|+ \t]"))

(defvar org-tblfm-rx (create-scanner "^[ \t]*#\\+TBLFM: "))

(defvar org-footnote-definition-rx (create-scanner "^\\[fn:\\([-_[:word:]]+\\)\\]"))

(defvar org-list-full-item-rx
  (create-scanner (concatenate 'string "^[ \t]*\\(\\(?:[-+*]\\|\\(?:[0-9]+\\|[A-Za-z]\\)[.)]\\)\\(?:[ \t]+\\|$\\)\\)"
	                       "\\(?:\\[@\\(?:start:\\)?\\([0-9]+\\|[A-Za-z]\\)\\][ \t]*\\)?"
	                       "\\(?:\\(\\[[ X-]\\]\\)\\(?:[ \t]+\\|$\\)\\)?"
	                       "\\(?:\\(.*\\)[ \t]+::\\(?:[ \t]+\\|$\\)\\)?"))
  "Matches a list item and puts everything into groups:
group 1: bullet
group 2: counter
group 3: checkbox
group 4: description tag")

(defvar org-item-rx
  (create-scanner "^[ t]*\\(\\(?:[-+*]\\|\\(?:[0-9]+\\|[A-Za-z]\\)[.)]\\)\\(?:[ t]+\\|$\\)\\)\\(?:\\[@\\(?:start:\\)?\\([0-9]+\\|[A-Za-z]\\)\\][ t]*\\)?\\(?:\\(\\[[ X-]\\]\\)\\(?:[ t]+\\|$\\)\\)?\\(?:\\(.*\\)[ t]+::\\(?:[ t]+\\|$\\)\\)?")
  "The correct regular expression for plain lists.")

(defun org-duration-p (str)
  (or (scan org-duration-full-rx str)
      (scan org-duration-mixed-rx str)
      (scan org-duration-hmm-rx str)))

(defvar org-element-types
  '(babel-call center-block clock comment comment-block diary-sexp drawer
    dynamic-block example-block export-block fixed-width footnote-definition
    headline horizontal-rule inlinetask item keyword latex-environment
    node-property paragraph plain-list planning property-drawer quote-block
    section special-block src-block table table-row verse-block)
  "List of all org-element types provided by org-element.el in 'org-element-all-elements'")

(defvar org-element-objects
  '(bold citation citation-reference code entity export-snippet
    footnote-ref inline-babel-call inline-src-block italic
    line-break latex-fragment link macro radio-target stat-cookie
    strike-through subscript superscript table-cell target timestamp underline verbatim)
  "List of all org-element objects provided by org-element.el in 'org-element-all-objects'")

(sb-int:defconstant-eqx +org-element-keywords+ #("NAME" "RESULTS" "HEADER" "CAPTION" "PLOT")
  #'equalp)

;; entities, latex-frag, sscript
(sb-int:defconstant-eqx +org-minimal-objects+
    #(plain-text bold italic underline verbatim code strike-through
      latex-fragment entity superscript subscript)
  #'equalp)

;; all except table cells and citation-refs
(sb-int:defconstant-eqx +org-standard-objects+ 
  (sequence:concatenate #() +org-minimal-objects+ 
                        #(footnote-ref active-timestamp active-timestamp-range 
                          inactive-timestamp inactive-timestamp-range
                          stat-cookie))
  #'equalp)
