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

(defvar-local org-graph nil
  "The currently active graph of org nodes.")

(defcustom org-graph-db-init-script (join-paths company-source-directory "infra/scripts/org-db-init.lisp")
  "Path to a lisp script responsible for initializing the `org-graph-db-directory'.")

(cl-defstruct org-graph-db-handle
  (type :rocksdb)
  (name "org-graph-db")
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
  (setq-local org-graph (copy-hash-table (org-id-locations-load)))
  (maphash
   (lambda (k v)
     (mapc
      (lambda (x)
        (unless (string-prefix-p x (file-truename v))
          (remhash k org-graph)))
      org-graph-locations))
   org-graph))

(provide 'graph)
;; graph.el ends here

