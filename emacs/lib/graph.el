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
(require 'default)
(require 'ulang)

(defgroup graph nil
  "CC Graph")

(defcustom org-graph-db-directory (join-paths user-org-stash-directory "graph")
  "graph database storage directory."
  :type 'directory
  :group 'graph)

(defcustom org-graph-locations (list (join-paths company-org-directory "notes/"))
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

(defcustom org-graph-db-init-script (join-paths company-source-directory "infra/scripts/org-db-init.lisp")
  "Path to a lisp script responsible for initializing the `org-graph-db-directory'.")

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

(defun org-graph-from-id-locations (&optional edges local)
  "Populate the `org-graph' from `org-id-locations', filtering out any
entries not under a member of `org-graph-locations'. When EDGES is
non-nil visit each node and collect all edges found."
  (interactive)
  (save-excursion
    (let* ((node-ids (org-id-locations-load))
           (graph (make-org-graph :nodes node-ids)))
      (maphash
       (lambda (k v)
         (if-let ((ok (cl-loop for l in org-graph-locations
                               when (string-prefix-p l (file-truename v))
                               return t)))
             (let ((pos (cdr (org-id-find-id-in-file k v))))
               (message "%s %s" k v)
               (org-with-file-buffer v
                 (goto-char pos)
                 (org-graph-node-at-point graph)
                 (when edges (org-graph-edges-at-point graph))))
           (remhash k (org-graph-nodes graph))))
       (org-graph-nodes graph))
      (if local
          (setq-local org-graph graph)
        (setq org-graph graph)))))

(defun org-graph-files ()
  (org-list-files org-graph-locations org-agenda-extensions))

(cl-defstruct org-graph
  ;; TODO 2024-09-17: use integers instead of string
  (nodes (make-hash-table :test 'equal))
  (edges (make-hash-table :test 'equal)))

(defvar org-graph (make-org-graph)
  "The Emacs-native org-graph. Should be assigned to an `org-graph' instance.")

(cl-defstruct org-graph-node id name file point)
(cl-defstruct org-graph-edge (type 'link) in properties timestamp out)

(defun org-graph--file-hash (file)
  "Compute the hash of FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'md5 (current-buffer))))

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
    (when update
      (puthash (org-graph-node-id node) node (org-graph-nodes (if (eql t update) org-graph update))))
    node))

(defun org-graph-collect-edge ()
  "Collect the edge at point which should be a line created with `org-graph-edge--insert'."
  (org-with-point-at (beginning-of-line)
    (when (org-at-timestamp-p 'lax)
      (let ((ts (match-string-no-properties 0))
            (end (match-end 0)))
        (goto-char (1+ end))
        ;; next 2 chars are the arrow
        (let ((arrow (org-graph-edge-arrow* (buffer-substring-no-properties (point) (1+ (point))))))
          (goto-char (+ (point) 4))
          (make-org-graph-edge :in (org-id-get)
                               :type arrow
                               :timestamp (org-parse-time-string ts t) 
                               :out (string-trim (org--link-at-point) "id:")))))))

(defun org-graph-collect-edges-at-point (&optional update)
  "Collect the contents of the EDGES drawer from node at point. When UPDATE
is non-nil insert or update the node into the org-graph object specified
or when 't' use the currently active org-graph."
  (with-org-graph-edge-drawer (end)
    (re-search-backward (rx bol ?: (literal (org-graph-edge-drawer)) ?: eol) nil t)
    (goto-char (1+ (match-end 0)))
    (let ((edges
           (cl-loop while (> (point-max) end (point))
                    collect (org-graph-collect-edge)
                    do (next-line))))
      (when update
        (mapc (lambda (e)
                (puthash 
                 (org-graph-edge-in e) 
                 e 
                 (org-graph-edges (if (eql t update) org-graph update))))
              edges))
      edges)))

(defun org-graph-edges-at-point (&optional update)
  "Return a list of `org-graph-edge' instances associated with the node at
point. When UPDATE is non-nil insert or update the edges into the
currently active org-graph."
  (interactive)
    (when (derived-mode-p 'org-mode)
      (org-graph-collect-edges-at-point update)))

(defun org-graph-buffer-update (&optional buffer)
  "Map over an org buffer adding all nodes to the active org-graph."
  (interactive)
  (save-excursion
    (with-current-buffer (or buffer (current-buffer))
      ;; capture file node
      (goto-char (point-min))
      (org-graph-node-at-point t)
      (when (derived-mode-p 'org-mode)
        (org-map-entries (lambda () (org-graph-node-at-point t)))))))

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

Default is the function `org-graph-edge-prefix-timestamp'
which returns an inactive timestamp formatted according to the variable
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

(defvar org-graph-edge-search-function 'org-graph-edge-get-location
  "The interface to use for finding target links. If you provide a custom
function it will be called with the `point` at the location the link
should be inserted.  The only other requirement is that it should call
the function `org-graph-edge--insert-link' with a marker to the target
link. AKA the place you want the edge.

`org-graph-edge-get-location' internally uses `org-refile-get-location'.")

(defvar org-graph-edge-pre-link-hook nil
  "Hook called before storing the link on the link side.
This is called with point at the location where it was called.")

(defvar org-graph-edge-pre-backlink-hook nil
  "Hook called before storing the link on the backlink side.
This is called with point in the heading of the backlink.")

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

(defun org-graph-edge-get-location ()
  "Default for function `org-graph-edge-search-function' that reuses the `org-refile' machinery."
  (let ((target (org-refile-get-location "Node")))
    (org-graph-edge--insert-link (set-marker (make-marker) (car (cdddr target))
                                             (get-file-buffer (car (cdr target)))))))

(cl-defmacro with-org-graph-edge-drawer ((start &optional create) &rest body)
  "START is a symbol which is bound to the start of the edge drawer."
  (declare (indent 1))
  `(save-excursion
     (org-with-wide-buffer
      (let ((org-log-into-drawer (org-graph-edge-drawer)))
        (org-graph-edge--org-narrow-to-here)
        (let ((,start (org-log-beginning ,create)))
          (when (re-search-forward (rx bol ?: "END" ?: eol) nil t)
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
  (let ((p (org-entry-get nil "EDGE_DRAWER" 'inherit t)))
    (cond ((equal p "nil") nil)
          ((stringp p) p)
          (t org-graph-edge-drawer))))

(defun org-graph-edge--org-narrow-to-here ()
  "Narrow to current heading, excluding subheadings."
  (org-narrow-to-subtree)
  (save-excursion
    (org-next-visible-heading 1)
    (narrow-to-region (point-min) (point))))

;; delete related functions
(defun org-graph-find-edges (id)
  "Return link elements for ID."
    (org-graph-edge--org-narrow-to-here)
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
  "Insert edge to LINK with DESC.
Where the edge is placed is determined by the variable `org-graph-edge-drawer'."
  (with-org-graph-edge-drawer (beg t)
    (let ((description (org-graph-edge-default-description-formatter link desc)))
      (org-graph-edge--insert link description 'backlink)
      (org-indent-region beg (point)))))

(defun org-graph-edge-insert-link (link desc)
  "insert a forward link edge."
  (with-org-graph-edge-drawer (beg t)
    (org-graph-edge--insert link desc 'link)                                  
    (org-indent-region beg (point))))

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

(defun org-graph-edge--insert-link (target &optional no-forward)
  "Insert link to marker TARGET and create an edge.
Only create edges in files in `org-mode' or a derived mode, otherwise just
act like a normal link.

If NO-FORWARD is non-nil skip creating the forward link.  Currently
only used when converting a link."
  (let* ((source (point-marker))
         (source-link (org-graph-edge-links-action source 'org-graph-edge-pre-link-hook))
         (target-link (org-graph-edge-links-action target 'org-graph-edge-pre-backlink-hook))
         (source-formatted-link (org-graph-edge-link-builder source-link))
         (target-formatted-link (org-graph-edge-link-builder target-link)))
    (with-current-buffer (marker-buffer target)
      (save-excursion
        (save-restriction
          (widen) ;; buffer could be narrowed
          (goto-char (marker-position target))
          (when (derived-mode-p 'org-mode)
            (org-graph-edge-insert-backlink (car source-formatted-link) (cdr source-formatted-link))))))
    (unless no-forward
      (with-current-buffer (marker-buffer source)
        (save-excursion
          (goto-char (marker-position source))
          (org-graph-edge-insert-link (car target-formatted-link) (cdr target-formatted-link)))))))

;;;###autoload
(defun org-graph-edge-convert-link (arg)
  "Convert a normal `org-mode' link at `point' to a graph link, ARG prefix.
When called interactively with a `C-u' prefix argument do not modify existing link."
  (interactive "P")
  (let ((from-m (point-marker))
        (target (save-window-excursion
                  (with-current-buffer (current-buffer)
                    (save-excursion
                      (org-open-at-point)
                      (point-marker))))))
    (org-graph-edge--insert-link target arg)
    (goto-char (marker-position from-m)))
  (when (not arg)
    (let ((begin (org-element-property :begin (org-element-context)))
          (end (org-element-property :end (org-element-context))))
      (delete-region begin end))))

;;;###autoload
(defun org-graph-edge-delete ()
  "Delete the link at point, and the corresponding reverse link.
If no reverse link exists, just delete link at point.
This works from either side, and deletes both sides of a link."
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
  "Insert an edge from `org-stored-links')"
  (interactive)
  (if org-stored-links
      (progn
        (org-link-open (pop org-stored-links))
        (org-graph-edge--insert-link (set-marker (make-marker) (point))))
    (org-graph-edge-link)))

;;;###autoload
(defun org-graph-edge-link ()
  "Insert a link edge and add a backlink edge to the target heading."
  (interactive)
  (org-graph-edge-search-function))

(defun org-dblock-write:links ()
  "Generate a 'links' block for the designated node.")

(defun org-dblock-write:graph ()
  "Generate a 'graph' block for the designated set of nodes.")

(provide 'graph)
;; graph.el ends here
