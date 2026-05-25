;;; graph.el --- Graph-oriented Extensions -*- lexical-binding: t; -*-

;; Copyright (C) 2024  The Compiler Company
;; Version: "0.2.0"
;; Author: Richard Westhaver <richard.westhaver@gmail.com>
;; Keywords: docs, maint, outlines, extensions

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
(require 'org-agenda)
(require 'org-web-tools)
(require 'default)
(require 'ulang)

(defgroup graph nil
  "CC Graph"
  :group 'org)

(defcustom org-graph-db-directory (join-paths user-org-stash-directory "graph")
  "graph database storage directory."
  :type 'directory
  :group 'graph)

(defcustom org-graph-root (join-paths company-org-directory "graph/")
  "Location of the root graph directory."
  :type 'directory
  :group 'graph)

(defcustom org-graph-locations (list org-graph-root)
  "List of directories to check for nodes."
  :type '(list directory)
  :group 'graph)

(defcustom org-graph-include-agenda-files nil
  "When non-nil, include `org-agenda-files' in the graph."
  :type 'boolean
  :group 'graph)

(defcustom org-graph-include-archive nil
  "When non-nil, include `org-arhive-location' in the graph."
  :type 'boolean
  :group 'graph)

(defcustom org-graph-include-org-directory nil
  "When non-nil, include `org-directory' files in the graph."
  :type 'boolean
  :group 'graph)

(defcustom org-graph-compaction-hook nil
  "Hook run when a graph is compacted to `org-graph-db'."
  :type 'hook
  :group 'graph)

(defcustom org-graph-capture-hook nil
  "Hook run when a node is added to the graph."
  :type 'hook
  :group 'graph)

(defvar org-graph-target-maxlevel 4)

(defcustom org-graph-file (join-paths user-emacs-directory "graph.sxp")
  "Path to the default output location of 'org-graph-save'."
  :type 'file)

(cl-defstruct org-graph-db-handle
  (type :rocksdb)
  (name "org-graph-db")
  init
  get
  put
  delete
  merge
  compact
  shutdown)

(defcustom org-graph-db (make-org-graph-db-handle)
  "A handle to the database backend which stores nodes and edges."
  :type 'org-graph-db-handle
  :group 'graph)

(defcustom org-graph-ui-file (join-paths org-graph-root "ui.lisp")
  "Relative path to the optional UI file in org-graph-directory."
  :type 'file
  :group 'graph)

(defun org-graph-from-files (&optional files)
  (interactive)
  (let ((files (or files (org-graph-files t))))
    (cl-loop for c in files
	     do (org-graph-buffer-update c))))

(defun org-graph-file-p (v)
  (when v
    (cl-loop for l in org-graph-locations
	     when (string-prefix-p l (file-truename v))
	     return t)))

(defun org-graph-from-id-locations (&optional edges local)
  "Populate the `org-graph' from `org-id-locations', filtering out any
entries not under a member of `org-graph-locations'. When EDGES is
non-nil visit each node and collect all edges found."
  (interactive "P")
  (save-excursion
    (let* ((node-ids (copy-hash-table (or org-id-locations (org-id-locations-load)))) ;; don't overwrite `org-id-locations'
           (graph (make-org-graph :nodes node-ids)))
      (maphash
       (lambda (k v) 
	 (unless (org-graph-file-p v)
	   (remhash k node-ids)))
       node-ids)
      (let* ((total (hash-table-count node-ids))
	     (i 0)
	     (prog (make-progress-reporter "Building org-graph..."
					   i total)))
	(maphash
	 (lambda (k v)
	   (message "org-graph-node: %s:%s" v k)
	   (progress-reporter-update prog (incf i) v)
           (let ((pos (cdr (org-id-find-id-in-file k v))))
             (if pos
		 (progn
                   (org-with-file-buffer v   
                     (goto-char pos)
                     (org-graph-node-at-point graph)
                     (org-graph-edges-at-point graph)))
               (warn "couldn't find node %s %s" k v))))
	 (org-graph-nodes graph))
	(progress-reporter-done prog))
      (if local
          (setq-local org-graph graph)
        (setq org-graph graph)))))

(cl-defstruct org-graph
  ;; TODO 2024-09-17: use integers instead of string?
  (nodes (make-hash-table :test 'equal))
  (edges (make-hash-table :test 'equal)))

(defvar org-graph (make-org-graph)
  "The Emacs-native org-graph. Should be assigned to an `org-graph' instance.")

(cl-defstruct org-graph-node id name file point properties)
(cl-defmethod unwrap ((self org-graph-node))
  (with-slots (id name file point properties) self
    (list id name file point properties)))
(cl-defmethod wrap ((self org-graph-node) form)
  (dolist (s '(id name file point properties) self)
    (oset self s (pop form))))

(cl-defstruct org-graph-edge (type 'link) in properties timestamp point out)
(cl-defmethod unwrap ((self org-graph-edge))
  (with-slots (type in out timestamp point properties) self
    (list type in out timestamp point properties)))
(cl-defmethod wrap ((self org-graph-edge) form)
  (dolist (s '(type in out timestamp point properties) self)
    (oset self s (pop form))))

;; TODO 2025-03-03: b3hash
(defun org-graph--file-hash (file)
  "Compute the hash of FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'md5 (current-buffer))))

(defun org-graph-edge-list (&optional graph)
  (interactive)
  (hash-table-values (org-graph-edges (or graph org-graph))))

(defun org-graph-node-list (&optional graph)
  (interactive)
  (hash-table-values (org-graph-nodes (or graph org-graph))))

(defun org-graph-node-at-point (&optional update)
  "Return the `org-graph-node' at point. When UPDATE is non-nil insert or
update the node into the org-graph object specified or when 't' use the
currently active org-graph."
  (let* ((file (buffer-file-name))
         (node (make-org-graph-node :point (point) :file file)))
    (if (derived-mode-p 'org-mode)
        (progn
          (if (org-before-first-heading-p)
              (setf (org-graph-node-name node) (org-get-title)
                    ;; use the filename, create a hash as id
                    (org-graph-node-id node) (org-graph--file-hash file))
            (setf (org-graph-node-id node) (org-id-get)
                  (org-graph-node-name node) (elt (org-heading-components) 4))))
      (setf (org-graph-node-id node) (org-graph--file-hash file)
            (org-graph-node-name node) (file-name-nondirectory file)))
    (setf (org-graph-node-properties node) 
	  `(,@(let ((ts (org-entry-get (point) "CREATED")))
		(when ts `(:created ,(org-parse-time-string ts t))))
            ,@(let ((tags (org-entry-get (point) "ALLTAGS"))) 
		(when tags 
		  `(:tags 
		    ,(split-string 
		      (substring-no-properties tags)
		      ":" t))))
	    ,@(let ((aka (org-entry-get (point) "AKA")))
		(when aka
		  `(:aka ,aka)))))
    (when update
      (puthash (org-graph-node-id node) node (org-graph-nodes (if (eql t update) org-graph update))))
    node))

;; TODO 2024-09-22: properties
(defun org-link--description-at-point ()
  (interactive)
  (let ((link (org-element-context)))
    (buffer-substring-no-properties (org-element-property :contents-begin link)
				    (org-element-property :contents-end link))))

(defun org-graph-collect-edge ()
  "Collect the edge at point which should be a line created with `org-graph-edge--insert'."
  (org-with-point-at (beginning-of-line)
    (when (org-at-timestamp-p 'lax)
      (let ((ep (point))
            (ts (match-string-no-properties 0))
            (end (match-end 0)))
        (goto-char (1+ end))
        ;; next 2 chars are the arrow
        (let ((arrow (org-graph-edge-arrow* (buffer-substring-no-properties (point) (+ 2 (point))))))
          (goto-char (+ (point) 4))
          (make-org-graph-edge :in (org-id-get)
                               :type arrow
                               :point ep
                               :timestamp (org-parse-time-string ts t)
			       :properties `(:name ,(org-link--description-at-point))
                               :out (string-trim (org--link-at-point) "id:")))))))

(defun org-graph-map-edges (function)
  "Eval FUNCTION once for each edge in node at point with point at start of the edge."
  (save-excursion
    (with-org-graph-edge-drawer (end)
      (unless (eobp)
	(org-fold-reveal))
      (re-search-backward (rx bol ?: (literal (org-graph-edge-drawer)) ?: eol) nil t)
      (goto-char (1+ (match-end 0)))
      (cl-loop while (> (point-max) end (point))
               for x = (funcall function)
	       unless (not x) 
	       collect x
               do (next-line)))))

;; TODO 2024-09-23: 
(defun org-link-info (link)
  (let ((path (org-element-property :path link))
        (type (org-element-property :type link))
        (desc (substring-no-properties (nth 2 link))))
    (list type path desc)))

;; TODO 2024-09-22: 
(defun org-graph-infer-edges ()
  "Infer edges from the contents of the node at point. The result of this
function is a list of org-graph-edge objects."
  ;; collect links
  (with-org-graph-edge-drawer (beg)
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (print link)
        ;; (org-graph-edge-link-builder (funcall 'org-element-create link))
        ))))

(defun org-graph-reduce-edges (function)
  "Same as `cl-reduce' where SEQ is the list of edges at point. FUNCTION
takes two `org-graph-edge' objects as input."
  (let ((edges (org-graph-map-edges 'org-graph-collect-edge)))
    (cl-reduce function edges)))

(defun org-graph-collect-edges-at-point (&optional update)
  "Collect the contents of the EDGES drawer from node at point. When UPDATE
is non-nil insert or update the node into the org-graph object specified
or when 't' use the currently active org-graph."
  (let ((edges (org-graph-map-edges 'org-graph-collect-edge)))
    (when update
      (mapc (lambda (e)
	      (puthash 
	       (org-graph-edge-in e)
	       e
	       (org-graph-edges (if (eql t update) org-graph update))))
            edges))
    edges))

(defun org-graph-edge-equal (a b)
  "Return non-nil if A and B are 'equal' org-graph-edge objects."
  (equal (org-graph-edge-out a) (org-graph-edge-out b)))

(defun org-graph-edge-remove-duplicates ()
  "Remove duplicate edge entries from node at point."
  (org-graph-reduce-edges 
   (lambda (a b) 
     (when (org-graph-edge-equal a b)
       (let ((tsa (org-graph-edge-timestamp a))
             (tsb (org-graph-edge-timestamp b)))
         (goto-char (org-graph-edge-point (if (org-time> tsa tsb) b a)))
         (delete-line))))))

(defun org-graph-edges-at-point (&optional update)
  "Return a list of `org-graph-edge' instances associated with the node at
point. When UPDATE is non-nil insert or update the edges into the
currently active org-graph."
  (interactive)
  (org-graph-collect-edges-at-point update))

(defun org-graph-buffer-update (&optional buffer)
  "Map over an org buffer adding all nodes to the active org-graph."
  (interactive)
  (org-with-file-buffer (or buffer (buffer-file-name))
    
    ;; capture file node
    (goto-char (point-min))
    (org-graph-node-at-point t)
    (when (derived-mode-p 'org-mode)
      (org-map-entries 
       (lambda ()
	 (org-graph-node-at-point t)
	 (org-graph-edges-at-point t))))))

;;; Edges
;; See https://github.com/toshism/org-super-links/blob/develop/org-super-links.el
(declare-function org-make-link-description-function "ext:org-mode")

(defvar org-graph-edge-drawer "EDGES"
  "Controls how/where to insert edges. If nil edges will just be inserted
under the heading.")

;; TODO 2024-09-16: edge properties
(defvar org-graph-edge-prefix 'org-graph-edge-prefix-timestamp
  "Prefix to insert before the edge.
This can be a string, nil, or a function that takes no arguments and
returns a string.

Default is the function `org-graph-edge-prefix-timestamp' which returns
an inactive timestamp formatted according to the variable
`org-time-stamp-formats'.")

;;  TODO 2024-09-16: do we need this? what sort of information for a
;;  given edge would go in the postfix? this may be better suited as a
;;  per-edge value rather than global - maybe use for comments.
(defvar org-graph-edge-postfix nil
  "Postfix to insert after the edge.
This can be a string, nil, or a function that takes no arguments and
returns a string")

(defvar org-graph-edge-link-prefix nil
  "Prefix to insert before the link.
This can be a string, nil, or a function that takes no arguments and
returns a string")

(defvar org-graph-edge-link-postfix nil
  "Postfix to insert after the link.
This can be a string, nil, or a function that takes no arguments and
returns a string")

(defvar org-graph-edge-default-description-formatter org-make-link-description-function
  "What to use if no description is provided.
This can be a string, nil or a function that accepts two arguments
LINK and DESC and returns a string.

nil will return the default desciption or the link.
string will be used only as a default fall back if set.
function will be called for every link.

Default is the variable `org-make-link-desciption-function'.")

(defvar org-graph-edge-search-function 'org-graph-get-location
  "The interface to use for finding target links. If you provide a custom
function it will be called with the `point` at the location the link
should be inserted.  The only other requirement is that it should call
the function `org-graph-edge-insert-link-marker' with a marker to the target
link. AKA the place you want the edge.

`org-graph-edge-get-location' internally uses `org-refile-get-location'.")

(defvar org-graph-edge-pre-link-hook nil
  "Hook called before storing the link on the link side.
This is called with point at the location where it was called.")

(defvar org-graph-edge-pre-backlink-hook nil
  "Hook called before storing the link on the backlink side.
This is called with point in the heading of the backlink.")

(defvar org-graph-pre-child-hook nil)
(defvar org-graph-pre-parent-hook nil)

(defvar org-graph-edge-indicator-alist
  '((link . "->")
    (backlink . "<-")
    (relation . "--")
    (parent . ">>")
    (child . "<<"))
  "An alist of (EDGE-TYPE . INDICATOR) pairs. Each INDICATOR is a string
which will be printed between the properties and backlink of the
associated EDGE-TYPE.")

(defun org-graph-edge-arrow (sym)
  (cdr (assoc sym org-graph-edge-indicator-alist)))

(defun org-graph-edge-arrow* (str)
  "Reverse lookup of edge arrow symbol."
  (car (rassoc str org-graph-edge-indicator-alist)))

(defun org-graph-get-node (id) (gethash id (org-graph-nodes org-graph)))
(defun org-graph-get-edges (id) (gethash id (org-graph-edges org-graph)))

(defun org-graph-get-location ()
  "Prompt the user for a graph node location using PROMPT."
  (let ((names) (ids))
    (maphash (lambda (k v) 
	       (push (org-graph-node-name v) names)
	       (push (org-graph-node-id v) ids))
	     (org-graph-nodes org-graph))
    (let ((node (org-graph-get-node 
		 (elt ids (cl-position (completing-read "refile node to: " names) names :test 'string=)))))
      (set-marker (make-marker) (org-graph-node-point node) (find-file-noselect (org-graph-node-file node))))))

(defun org-graph-refile-get-location ()
  "`org-graph-edge-search-function' that reuses the `org-refile' machinery."
  (car (cdddr (org-refile-get-location "Node"))))

(cl-defmacro with-org-graph-edge-drawer ((start &optional create) &rest body)
  "START is a symbol which is bound to the start of the edge drawer."
  (declare (indent 1))
  `(save-excursion
     (org-with-wide-buffer
      (let ((org-log-into-drawer (org-graph-edge-drawer)))
        (org-graph-narrow-to-node)
        (let ((,start (org-log-beginning ,create)))
          (when (or (re-search-forward (rx bol ?: "END" ?: eol) nil t)
                    (re-search-backward (rx bol ?: "END" ?: eol) nil t))
            (goto-char ,start)
            ,@body))))))

(defun org-graph-edge-search-function ()
  "Call the search interface specified in variable `org-graph-edge-search-function'."
  (funcall org-graph-edge-search-function))

(defun org-graph-edge-prefix ()
  "Return an appropriate string based on variable `org-graph-edge-prefix'."
  (cond ((equal org-graph-edge-prefix nil) "")
        ((stringp org-graph-edge-prefix) org-graph-edge-prefix)
        (t (funcall org-graph-edge-prefix))))

(defun org-graph-edge-postfix ()
  "Return an appropriate string based on variable `org-graph-edge-postfix'."
  (cond ((equal org-graph-edge-postfix nil) "\n")
        ((stringp org-graph-edge-postfix) org-graph-edge-postfix)
        (t (funcall org-graph-edge-postfix))))

(defun org-graph-edge-link-prefix ()
  "Return an appropriate string based on variable `org-graph-edge-link-prefix'."
  (cond ((equal org-graph-edge-link-prefix nil) "")
        ((stringp org-graph-edge-link-prefix) org-graph-edge-link-prefix)
        (t (funcall org-graph-edge-link-prefix))))

(defun org-graph-edge-link-postfix ()
  "Return an appropriate string based on variable `org-graph-edge-link-postfix'."
  (cond ((equal org-graph-edge-link-postfix nil) "")
        ((stringp org-graph-edge-link-postfix) org-graph-edge-link-postfix)
        (t (funcall org-graph-edge-link-postfix))))

;; TODO 2024-09-16: edge-properties
(defun org-graph-edge-prefix-timestamp ()
  "Return the default prefix string for an edge.
Inactive timestamp formatted according to `org-time-stamp-formats'."
  (format-time-string (org-time-stamp-format t t) (current-time)))

(defun org-graph-edge-default-description-formatter (link desc)
  "Return a string to use as the link desciption.
LINK is the link target.  DESC is the provided desc."
  (let ((p org-graph-edge-default-description-formatter))
    (cond ((equal p nil) (or desc link))
          ((stringp p) (or desc p))
          ((fboundp p) (funcall p link desc))
          (t desc))))

(defun org-graph-edge-drawer ()
  "Name of the edge drawer, as a string, or nil.
This is the value of variable
`org-graph-edge-drawer'.  However, if the current
entry has or inherits a EDGE_DRAWER property, it will be
used instead of the default value."
  (let ((p (org-entry-get nil "EDGE_DRAWER" 'inherit)))
    (cond ((stringp p) p)
          (t org-graph-edge-drawer))))

(defun org-graph-narrow-to-node ()
  "Narrow to current heading, excluding subheadings."
  (org-narrow-to-subtree)
  (save-excursion
    (org-next-visible-heading 1)
    (narrow-to-region (point-min) (point))))

;; delete related functions
(defun org-graph-find-links (id)
  "Return link elements for ID."
  (org-graph-narrow-to-node)
  (let ((links
         (org-element-map (org-element-parse-buffer) 'link
           (lambda (link)
             (when (string= (org-element-property :path link) id)
               link)))))
    (widen)
    links))

(defun org-graph-edge--in-drawer-p ()
  "Return non-nil if point is in drawer. Value is element at point."
  (let ((element (org-element-at-point)))
    (while (and element
                (not (memq (org-element-type element) '(drawer property-drawer))))
      (setq element (org-element-property :parent element)))
    element))

(defun org-graph-edge--delete-link (link)
  "Delete the LINK. If point is in edges drawer, delete the entire line."
  (save-excursion
    (goto-char (org-element-property :begin link))
    (if (org-graph-edge--in-drawer)
        (progn
          (kill-whole-line 1)
          (org-remove-empty-drawer-at (point)))
      (delete-region (org-element-property :begin link) (org-element-property :end link)))))

(defun org-graph-edge--insert (link desc arrow &rest props)
  "Insert an edge at point. ARROW is a symbol representing the type of
arrow to insert. The rest of the arguments are parsed as :KEY VAL pairs
which are inserted with the edge."
  (insert (format "%s %s " (org-graph-edge-prefix)
                  (org-graph-edge-arrow arrow)))
  (org-insert-link nil link desc)
  (insert (org-graph-edge-link-postfix))
  (newline))

(defun org-graph-edge-insert-related (link desc)
  "Insert a relation edge."
  (with-org-graph-edge-drawer (beg t)
    (org-graph-edge--insert link desc 'relation)
    (org-indent-region beg (point))))

(defun org-graph-edge-insert-backlink (link desc)
  "Insert a backlink edge."
  (with-org-graph-edge-drawer (beg t)
    (let ((description (org-graph-edge-default-description-formatter link desc)))
      (org-graph-edge--insert link description 'backlink)
      (org-indent-region beg (point)))))

(defun org-graph-edge-insert-link (link desc)
  "insert a forward link edge. When BACKLINK is non-nil also create a
backlink at the node specified in LINK."
  (with-org-graph-edge-drawer (beg t)
    (let ((description (org-graph-edge-default-description-formatter link desc)))
      (org-graph-edge--insert link desc 'link)
      (org-indent-region beg (point)))))

(defun org-graph-edge-insert-parent (link desc)
  "insert a forward link edge. When BACKLINK is non-nil also create a
backlink at the node specified in LINK."
  (with-org-graph-edge-drawer (beg t)
    (let ((description (org-graph-edge-default-description-formatter link desc)))
      (org-graph-edge--insert link desc 'parent)
      (org-indent-region beg (point)))))

(defun org-graph-edge-insert-child (link desc)
  "insert a forward link edge. When BACKLINK is non-nil also create a
backlink at the node specified in LINK."
  (with-org-graph-edge-drawer (beg t)
    (let ((description (org-graph-edge-default-description-formatter link desc)))
      (org-graph-edge--insert link desc 'child)
      (org-indent-region beg (point)))))

(defun org-graph-edge-links-action (marker hooks)
  "Go to MARKER, run HOOKS and store a link."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (save-restriction
        (widen) ;; buffer could be narrowed
        (goto-char (marker-position marker))
        (run-hooks hooks)
        (call-interactively #'org-store-link)
        (pop org-stored-links)))))

(defun org-graph-edge-link-builder (link)
  "Format link description for LINK."
  (let* ((link-ref (car link))
         (pre-desc (cadr link))
         (description (org-graph-edge-default-description-formatter link-ref pre-desc)))
    (cons link-ref description)))

(defun org-graph-edge-insert-child-marker (target &optional no-parent)
  "Insert link to marker TARGET and create a child edge.
Optionally skip inserting a parent node at the target with NO-PARENT."
  (let* ((source (point-marker))
         (source-link (org-graph-edge-links-action source 'org-graph-edge-pre-child-hook))
         (target-link (org-graph-edge-links-action target 'org-graph-edge-pre-parent-hook))
         (source-formatted-link (org-graph-edge-link-builder source-link))
         (target-formatted-link (org-graph-edge-link-builder target-link)))
    (unless no-parent
      (with-current-buffer (marker-buffer target)
        (save-excursion
          (save-restriction
            (widen) ;; buffer could be narrowed
            (goto-char (marker-position target))
            (when (derived-mode-p 'org-mode)
              (org-graph-edge-insert-parent (car source-formatted-link) (cdr source-formatted-link)))))))
    (with-current-buffer (marker-buffer source)
      (save-excursion
        (goto-char (marker-position source))
        (print target-formatted-link)
        (org-graph-edge-insert-child (car target-formatted-link) (cdr target-formatted-link))))))

(defun org-graph-edge-insert-parent-marker (target &optional no-child)
  "Insert link to marker TARGET and create a parent edge.
Optionally skip inserting a child node at the target with NO-CHILD."
  (let* ((source (point-marker))
	 (source-link (org-graph-edge-links-action source 'org-graph-edge-pre-parent-hook))
	 (target-link (org-graph-edge-links-action target 'org-graph-edge-pre-child-hook))
	 (source-formatted-link (org-graph-edge-link-builder source-link))
	 (target-formatted-link (org-graph-edge-link-builder target-link)))
    (unless no-child
      (with-current-buffer (marker-buffer target)
	(save-excursion
	  (save-restriction
	    (widen) ;; buffer could be narrowed
	    (goto-char (marker-position target))
	    (when (derived-mode-p 'org-mode)
	      (org-graph-edge-insert-child (car source-formatted-link) (cdr source-formatted-link)))))))
    (with-current-buffer (marker-buffer source)
      (save-excursion
	(goto-char (marker-position source))
	(print target-formatted-link)
	(org-graph-edge-insert-parent (car target-formatted-link) (cdr target-formatted-link))))))

(defun org-graph-edge-insert-link-marker (target &optional no-forward no-backward)
  "Insert link to marker TARGET and create an edge.
Only create edges in files in `org-mode' or a derived mode, otherwise just
act like a normal link.

If NO-FORWARD is non-nil skip creating the forward link. If NO-BACKWARD
is non-nil skip creating the backlink."
  (let* ((source (point-marker))
         (source-link (org-graph-edge-links-action source 'org-graph-edge-pre-link-hook))
         (target-link (org-graph-edge-links-action target 'org-graph-edge-pre-backlink-hook))
         (source-formatted-link (org-graph-edge-link-builder source-link))
         (target-formatted-link (org-graph-edge-link-builder target-link)))
    (unless no-backward
      (with-current-buffer (marker-buffer target)
        (save-excursion
          (save-restriction
            (widen) ;; buffer could be narrowed
            (goto-char (marker-position target))
            (when (derived-mode-p 'org-mode)
              (org-graph-edge-insert-backlink (car source-formatted-link) (cdr source-formatted-link)))))))
    (unless no-forward
      (with-current-buffer (marker-buffer source)
        (save-excursion
          (goto-char (marker-position source))
          (print target-formatted-link)
          (org-graph-edge-insert-link (car target-formatted-link) (cdr target-formatted-link)))))))

;;;###autoload
(defun org-graph-edge-convert-link (&optional arg)
  "Convert a normal `org-mode' link at `point' to a graph link, ARG prefix.
When called interactively with a `C-u' prefix argument do not modify
existing link."
  (interactive "P")
  (let ((from-m (point-marker))
        (target (save-window-excursion
                  (with-current-buffer (current-buffer)
                    (save-excursion
                      (org-open-at-point)
                      (point-marker))))))
    (org-graph-edge-insert-link-marker target arg)
    (goto-char (marker-position from-m)))
  (when (not arg)
    (let ((begin (org-element-property :begin (org-element-context)))
          (end (org-element-property :end (org-element-context))))
      (delete-region begin end))))

;;;###autoload
(defun org-graph-edge-delete ()
  "Delete the link at point, and the corresponding backlink.
If no backlink exists, just delete link at point. This works from
either side, and deletes both sides of a link."
  (interactive)
  (save-window-excursion
    (with-current-buffer (current-buffer)
      (save-excursion
        (let ((id (org-id-get (point))))
          (org-open-at-point)
          (let ((link-elements (org-graph-find-edges id)))
            (if link-elements
                (if (> (length link-elements) 1)
                    (error "Multiple links found.")
                  (org-graph-edge--delete-link (car link-elements)))
              (message "No edge found. Deleting active only.")))))))
  (org-graph-edge--delete-link (org-element-context)))

;;;###autoload
(defun org-graph-edge-insert ()
  "Insert an edge from `org-stored-links'."
  (interactive)
  (if org-stored-links
      (progn
        (org-link-open (pop org-stored-links))
        (org-graph-edge-insert-link-marker (set-marker (make-marker) (point))))
    (org-graph-edge-link)))

;;;###autoload
(defun org-graph-edge-link (&optional no-backlink)
  "Insert a link edge and add a backlink edge to the target heading. With
'C-u' don't create a backlink to the target."
  (interactive)
  (let ((target (org-graph-edge-search-function)))
    (org-graph-edge-insert-link-marker target nil no-backlink)))


;;;###autoload
(defun org-graph-node (&optional arg invisible-ok level)
  (interactive "P")
  (org-insert-heading arg invisible-ok level)
  (org-id-get-create)
  (org-expiry-insert-created))

(defun org-graph-files (&optional clean)
  (let ((files 
	 (flatten 
	  (mapcar (lambda (x) 
		    (let ((paths 
			   (cl-remove-if 
			    (lambda (y) (string-prefix-p "." y))
			    (directory-files x)))
			  (ret))
		      (dolist (d paths ret)
			(let ((xd (join-paths x d))) 
			  (if (file-directory-p xd)
			      (push (directory-files-recursively xd "**/*.org$") ret)
			    (push xd ret))))))
		  org-graph-locations))))
    (if clean
	(cl-remove-if '(lambda (x) 
			 (or
			  (string= (file-name-base x) "readme")
			  (string= (file-name-base x) "index")
			  (string= x org-graph-ui-file)
			  (not (string= (file-name-extension x) "org"))))
		      files)
      files)))

(defun org-graph--targets ()
  (cons (org-graph-files t) (cons :maxlevel org-graph-target-maxlevel)))

;;;###autoload
(defun org-graph-kill-all (&optional exclude-readme)
  (interactive)
  (mapcar
   (lambda (x)
     (when (and 
	    (member (buffer-file-name x) (org-graph-files exclude-readme))
	    (buffer-live-p x))
       (kill-buffer x)))
   (org-buffer-list))
  (message "closed all org-graph buffers"))

(defun org-graph-install-refile-targets ()
  (cl-pushnew (org-graph--targets) org-refile-targets 
	      :test (lambda (a b) (equal (car a) (car b))))
  (org-refile-get-targets))

;; TODO 2025-03-01: babel-mode or only no-readme?
;;;###autoload
(defun org-graph-init (&optional no-readme)
  (interactive)
  (org-graph-kill-all no-readme)
  (org-id-update-id-locations (org-graph-files))
  (org-id-locations-save)
  (org-graph-from-id-locations t))

;;;###autoload
(defun org-graph-load ()
  "Load the org-graph from the org-graph-db."
  (interactive))

(defun org-graph-node-edges (node)
  "Return the edges associated with NODE."
   (gethash (org-graph-node-id node) (org-graph-edges org-graph)))

(defun org-graph-tablist ()
  (mapcar 
   (lambda (x)
     (with-slots (id name file properties) x
       (list id 
	     `[,(if name (substring-no-properties name) "")
	       ,(if file (string-trim file org-graph-root) "")
	       ,(if #1=(plist-get properties :tags)
		  (if (stringp #1#) 
		      #1#
		    (apply 'concat (intersperse ":" #1#)))
		  "")
	       ,(format "%s" (let ((edges (org-graph-node-edges x)))
			       (mapcar (lambda (x)   
					 (with-slots (type out timestamp properties) x
					   (list (org-graph-edge-arrow type) out timestamp properties)))
				       (if (listp edges) edges (list edges)))))])))
   (org-graph-node-list)))

(defun org-graph-plist ()
  (list :nodes (mapcar 'unwrap (org-graph-node-list))
	:edges (mapcar 'unwrap (org-graph-edge-list))))

(defun org-graph-json ()
  (json-encode-plist (org-graph-plist)))

(defun org-graph-save (&optional output json)
  "Save the org-graph to a sxp file."
  (interactive)
  (with-temp-buffer
    (beginning-of-buffer)
    (if json (insert (org-graph-json)) 
      (pp (org-graph-plist) (current-buffer)))
    (write-file (or output org-graph-file))))

(defun org-graph-edge-backlink ()
  "Insert a backlink edge from the target to current heading."
  (interactive)
  (let ((target (org-graph-edge-search-function)))
    (org-graph-edge-insert-link-marker target t)))


(defun org-graph-edge-child (&optional no-parent)
  "Insert a child edge from the target to the current heading."
  (interactive "P")
  (let ((target (org-graph-edge-search-function)))
    (org-graph-edge-insert-child-marker target no-parent)))


(defun org-graph-edge-parent (&optional no-child)
  "Insert a parent edge to the current heading from the target."
  (interactive "P")
  (let ((target (org-graph-edge-search-function)))
    (org-graph-edge-insert-parent-marker
     target
     no-child)))

(defun org-graph-edge-web (&optional link desc)
  "Insert a related link to a web page."
  (interactive (list (org-web-tools--read-url)))
  (let ((desc 
         (or desc 
             (when link
               (if-let* ((dom (plz 'get link :as (lambda ()
                                                   (libxml-parse-html-region (point-min) (point-max)))
				:else nil))
                         (title (cl-caddr (car (dom-by-tag dom 'title)))))
                   (org-web-tools--cleanup-title title)
                 (message "HTML page at URL has no title"))))))
    (when link (org-graph-edge-insert-related link desc))))

(defun org-graph-edge-info (&optional link desc)
  "Insert a related link to an info page."
  (interactive "sinfo:")
  (when link (org-graph-edge-insert-related (format "info:%s" link) (or desc "info"))))

(defun org-graph-edge-man (&optional link desc)
  "Insert a related link to a man page."
  (interactive "sinfo:")
  (when link (org-graph-edge-insert-related (format "man:%s" link) (or desc "man"))))

(defun org-graph-edge-wikipedia (&optional link desc)
  "Insert a related link to a wikipedia page."
  (interactive "swiki:")
  (when link (org-graph-edge-insert-related (format "wikipedia:%s" link) (or desc "wiki"))))

(defun org-graph-edge-src (&optional link desc)
  "Insert a related source link."
  (interactive "ssrc:")
  (when link (org-graph-edge-insert-related link (or desc "src"))))

(defun org-graph-edge-github-src (&optional link desc)
  "Insert a related link to a github source."
  (interactive "ssrc:")
  (when link (org-graph-edge-insert-related (format "github:%s" link) (or desc "src"))))

;;; Dynamic Blocks
(defun org-dblock-write:links ()
  "Generate a 'links' block for the designated node.")

(defun org-dblock-write:graph ()
  "Generate a 'graph' block for the designated set of nodes.")

;;; Keys
(defvar org-graph-map-prefix "C-c g")

(defvar-keymap org-graph-map
  :doc "org-graph keymap"
  "n" 'org-graph-node
  "w" 'org-graph-edge-web
  "l" 'org-graph-edge-link
  "W" 'org-graph-edge-wikipedia
  "G" 'org-graph-edge-github-src
  "s" 'org-graph-edge-src
  "i" 'org-graph-edge-info
  "m" 'org-graph-edge-man
  "c" 'org-graph-edge-child
  "p" 'org-graph-edge-parent)

;;; Minor Mode
(define-minor-mode org-graph-minor-mode
  "Minor mode for `org-graph'."
  :lighter " OG"
  :group 'graph
  (keymap-local-set org-graph-map-prefix org-graph-map))

(defun org-graph-maybe-enable ()
  (when (org-graph-file-p buffer-file-name) (org-graph-minor-mode 1)))

(add-hook 'org-mode-hook 'org-graph-maybe-enable)

;;; Graph Menu Mode
(defcustom node-title-column-width 30
  "Column width for the Node title in the graph menu."
  :type 'natnum
  :group 'graph)

(defcustom node-edges-column-width 14
  "Column width for the Node edges in the graph menu."
  :type 'natnum
  :group 'graph)

(defcustom node-tags-column-width 14
  "Column width for the Node tags in the graph menu."
  :type 'natnum
  :group 'graph)

(defcustom node-file-column-width 32
  "Column width for the Node properties in the graph menu."
  :type 'natnum
  :group 'graph)

(defcustom node-properties-column-width 12
  "Column width for the Node properties in the graph menu."
  :type 'natnum
  :group 'graph)

(defcustom graph-async t
  "If non-nil, graph-menu will use async operations when possible."
  :type 'boolean
  :group 'graph)

(defun graph-menu--title-predicate (a b)
  (string< (aref (cadr a) 0) (aref (cadr b) 0)))

(defun graph-menu--file-predicate (a b)
  (string< (aref (cadr a) 1) (aref (cadr b) 1)))

(defun graph-menu--edges-predicate (a b))
(defun graph-menu--tags-predicate (a b))
(defun graph-menu--properties-predicate (a b))

;; TODO 2025-10-31: 
(defun graph-menu--populate ())

;; (defun graph-menu--refresh (&optional _arg _no-confirm))

(define-derived-mode graph-menu-mode tabulated-list-mode "Graph Menu"
  "Major mode for browsing a list of graph nodes."
  :interactive nil
  (setq tabulated-list-format
	`[("Title" ,node-title-column-width graph-menu--title-predicate)

	  ("File"  ,node-file-column-width  graph-menu--file-predicate)
	  ("Tags"  ,node-tags-column-width  graph-menu--tags-predicate)
	  ("Edges" ,node-edges-column-width graph-menu--edges-predicate)
	  ;; ("Properties" ,node-properties-column-width graph-menu--properties-predicate)
	  ])
  (setq-local tabulated-list-padding 2
	      ;; tabulated-list-sort-key (cons "Title" nil)
	      tabulated-list-entries (org-graph-tablist)
	      ;; revert-buffer-function 'graph-menu--refresh
	      )
  (tabulated-list-init-header)
  (tabulated-list-print))

(defun graph-list ()
  (interactive)
  (let ((buf (get-buffer-create "*Graph*")))
    (with-current-buffer buf
      ;; (setq buffer-file-coding-system 'utf-8)
      (graph-menu-mode))
    (pop-to-buffer-same-window buf)))

(provide 'graph)
;; graph.el ends here
