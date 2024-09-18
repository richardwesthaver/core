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

(defcustom org-graph-locations (list (join-paths company-org-directory "notes"))
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

(defun org-graph-from-id-locations ()
  "Populate the `org-graph' from `org-id-locations', filtering out any
entries not under a member of `org-graph-locations'."
  (interactive)
  (setq-local org-graph (copy-hash-table (org-id-locations-load)))
  (maphash
   (lambda (k v)
     (mapc
      (lambda (x)
        (unless (string-prefix-p x (file-truename v))
          (remhash k org-graph)))
      org-graph-locations))
   org-graph))

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
update the node into the currently active org-graph."
  (let* ((file (buffer-file-name))
         (node (make-org-graph-node :point (point) :file file)))
    (if (derived-mode-p 'org-mode)
        (progn
          (if (org-before-first-heading-p)
              (setf (org-graph-node-name node) (org-get-title)
                    ;; use the filename, create a hash as id
                    (org-graph-node-id node) (org-graph--file-hash file))
            (setf (org-graph-node-id node) (org-id-get)
                  (org-graph-node-name node) (cadddr (org-heading-components)))))
          (setf (org-graph-node-id node) (org-graph--file-hash file)
                (org-graph-node-name node) (file-name-nondirectory file)))
    (when update
      (puthash (org-graph-node-id node) node (org-graph-nodes org-graph)))
    (message "%s" node)))

;; TODO 2024-09-17: 
(defun org-graph-edges-at-point (&optional update)
  "Return a list of `org-graph-edge' instances associated with the node at
point. When UPDATE is non-nil insert or update the edges into the
currently active org-graph."
  (interactive)
  (let ((edges))
    (if (derived-mode-p 'org-mode))
    (when update
      (dolist (edge edges)
        (puthash (org-graph-edge-in edge) edge (org-graph-edges org-graph))))
    (message "%s" edge)))

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
  
;;; Links
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
`org-time-stamp-formats' and a separator ' <- '.")

;;  TODO 2024-09-16: do we need this? what sort of information for a
;;  given edge would go in the postfix? this may be better suited as a
;;  per-edge value rather than global - maybe use for comments.
(defvar org-graph-edge-postfix nil
  "Postfix to insert after the edge.
This can be a string, nil, or a function that takes no arguments and
returns a string")

(defvar org-graph-edge-related-into-drawer t
    "Controls how/where to insert links.
If non-nil a drawer will be created and links inserted there.  The
default is `org-graph-edge-related-drawer-default-name'.  If this is set to a
string a drawer will be created using that string.  For example LINKS.
If nil links will just be inserted at point.")

(defvar org-graph-edge-related-drawer-default-name "EDGES"
  "Default name to use for link drawer.
If variable `org-graph-edge-related-into-drawer' is 't' use this
name for the drawer.  See variable `org-graph-edge-related-into-drawer' for more info.")

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
    (sibling . "--")
    (parent . ">>")
    (child . "<<"))
  "An alist of (EDGE-TYPE . INDICATOR) pairs. Each INDICATOR is a string
which will be printed between the properties and backlink of the
associated EDGE-TYPE.")

(defun org-graph-edge-get-location ()
  "Default for function `org-graph-edge-search-function' that reuses the `org-refile' machinery."
  (let ((target (org-refile-get-location "Node")))
    (org-graph-edge--insert-link (set-marker (make-marker) (car (cdddr target))
                                 (get-file-buffer (car (cdr target)))))))

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
Inactive timestamp formatted according to `org-time-stamp-formats' and
a separator ' <- '."
  (concat (format-time-string (org-time-stamp-format t t) (current-time))
        " <- "))

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

;; delete related functions
(defun org-graph-edge--find-link (id)
  "Return link element for ID."
  (save-restriction
    (org-graph-edge--org-narrow-to-here)
    (let ((link
           (org-element-map (org-element-parse-buffer) 'link
             (lambda (link)
               (when (string= (org-element-property :path link) id)
                 link)))))
      (widen)
      (if (> (length link) 1)
          (error "Multiple links found.  Canceling delete")
        (car link)))))

(defun org-graph-edge--org-narrow-to-here ()
  "Narrow to current heading, excluding subheadings."
  (org-narrow-to-subtree)
  (save-excursion
    (org-next-visible-heading 1)
    (narrow-to-region (point-min) (point))))


(defun org-graph-edge--in-drawer ()
  "Return nil if point is not in a drawer.
Return element at point is in a drawer."
  (let ((element (org-element-at-point)))
    (while (and element
                (not (memq (org-element-type element) '(drawer property-drawer))))
      (setq element (org-element-property :parent element)))
    element))

(defun org-graph-edge--delete-link (link)
  "Delete the LINK.
If point is in drawer, delete the entire line."
  (save-excursion
    (goto-char (org-element-property :begin link))
    (if (org-graph-edge--in-drawer)
        (progn
          (kill-whole-line 1)
          (org-remove-empty-drawer-at (point)))
      (delete-region (org-element-property :begin link) (org-element-property :end link)))))

;;; EXPERIMENTAL 'related into drawer'
(defun org-graph-edge-related-into-drawer ()
  "Name of the related drawer, as a string, or nil.
This is the value of variable
`org-graph-edge-related-into-drawer'.  However, if the current
entry has or inherits a RELATED_INTO_DRAWER property, it will be
used instead of the default value."
  (let ((p (org-entry-get nil "RELATED_INTO_DRAWER" 'inherit t)))
    (cond ((equal p "nil") nil)
          ((equal p "t") org-graph-edge-related-drawer-default-name)
          ((stringp p) p)
          (p org-graph-edge-related-drawer-default-name)
          ((stringp org-graph-edge-related-into-drawer) org-graph-edge-related-into-drawer)
          (org-graph-edge-related-into-drawer org-graph-edge-related-drawer-default-name))))

(defun org-graph-edge-link-prefix-timestamp ()
  "Return the default prefix string for an edge.
Inactive timestamp formatted according to `org-time-stamp-formats' and
a separator ' -> '."
  (concat (format-time-string (org-time-stamp-format t t) (current-time))
          (format " %s " (cdr (assoc 'link org-graph-edge-indicator-alist)))))

(defun org-graph-edge-insert-related-link (link desc)
  "LINK DESC related experiment."
  (if (org-graph-edge-related-into-drawer)
      (let* ((org-log-into-drawer (org-graph-edge-related-into-drawer))
             (beg (org-log-beginning t)))
        (goto-char beg)
        (insert (org-graph-edge-link-prefix))
        (insert (org-graph-edge-link-prefix-timestamp))
        (org-insert-link nil link desc)
        (insert (org-graph-edge-link-postfix) "\n")
        (org-indent-region beg (point)))
    (insert (org-graph-edge-link-prefix))
    (org-insert-link nil link desc)
    (insert (org-graph-edge-link-postfix))))

(defun org-graph-edge-quick-insert-drawer-link ()
  "Insert link into drawer regardless of variable `org-graph-edge-related-into-drawer' value."
  (interactive)
  ;; how to handle prefix here?
  (let ((org-graph-edge-related-into-drawer (or org-graph-edge-related-into-drawer t))
        (org-graph-edge-link-prefix 'org-graph-edge-link-prefix-timestamp))
    (org-graph-edge-link)))

(defun org-graph-edge-quick-insert-inline-link ()
  "Insert inline link regardless of variable `org-graph-edge-related-into-drawer' value."
  (interactive)
  ;; how to handle prefix here?
  (let ((org-graph-edge-related-into-drawer nil)
        (org-graph-edge-link-prefix nil))
    (org-graph-edge-link)))

;; end

(defun org-graph-edge-insert (link desc)
  "Insert edge to LINK with DESC.
Where the edge is placed is determined by the variable `org-graph-edge-drawer'."
  (let* ((org-log-into-drawer (org-graph-edge-drawer))
         (description (org-graph-edge-default-description-formatter link desc))
         (beg (org-log-beginning t)))
    (goto-char beg)
    (insert (org-graph-edge-prefix))
    (insert (org-link-make-string link description))
    (insert (org-graph-edge-postfix))
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
  "Insert link to marker TARGET at current `point`, and create edge to here.
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
            (org-graph-edge-insert (car source-formatted-link) (cdr source-formatted-link))))))
    (unless no-forward
      (with-current-buffer (marker-buffer source)
        (save-excursion
          (goto-char (marker-position source))
          (org-graph-edge-insert-related-link (car target-formatted-link) (cdr target-formatted-link)))))))

;;;###autoload
(defun org-graph-edge-convert-link (arg)
  "Convert a normal `org-mode' link at `point' to a graph link, ARG prefix.
If variable `org-graph-edge-related-into-drawer' is non-nil move
the link into drawer.

When called interactively with a `C-u' prefix argument ignore
variable `org-graph-edge-related-into-drawer' configuration and
do not modify existing link."
  (interactive "P")
  (let ((from-m (point-marker))
        (target (save-window-excursion
                  (with-current-buffer (current-buffer)
                    (save-excursion
                      (org-open-at-point)
                      (point-marker))))))
    (org-graph-edge--insert-link target (or arg (not org-graph-edge-related-into-drawer)))
    (goto-char (marker-position from-m)))

  (when (and (not arg) (org-graph-edge-related-into-drawer))
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
          (let ((link-element (org-graph-edge--find-link id)))
            (if link-element
                (org-graph-edge--delete-link link-element)
              (message "No edge found. Deleting active only.")))))))
  (org-graph-edge--delete-link (org-element-context)))

(defvar org-graph-stored-mark nil
  "mark stored with `org-graph-edge-store'.")

(defun org-graph-edge-store ()
  "Store a point to register for use in function `org-graph-edge-insert-link'.
This is primarily intended to be called before `org-capture', but
could possibly even be used to replace `org-store-link' IF
function `org-graph-edge-insert-link' is used to replace
`org-insert-link'.  This has not been thoroughly tested outside
of links to/form org files."
  (interactive "P")
  (let ((c1 (make-marker)))
    (set-marker c1 (point) (current-buffer))
    (setq org-graph-stored-mark c1)
    (message "Mark stored.")))

;;;###autoload
(defun org-graph-edge-insert-link ()
  "Insert an edge from the list `org-graph-stored-marks'."
  (interactive)
  (if org-graph-stored-mark
      (progn 
        (org-graph-edge--insert-link org-graph-stored-mark)
        (setq org-graph-stored-mark nil))
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
