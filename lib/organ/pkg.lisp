;;; lib/organ/pkg.lisp --- Organ.

;; This package contains a parsing framework for Org Syntax. It
;; loosely follows the org-element.el conventions.

;;; Commentary:

;; Similar to the OG, we divide Org Syntax into two classes: 'objects'
;; and 'elements'. The paragraph is a useful unit of measurement.

;; Elements are syntactic components that exist at the same or greater
;; scope than a paragraph.

;; Objects are syntactic components that exist with a smaller scope
;; than a paragraph. All objects can be contained within a paragraph.

;; Expanding further, 'Lesser' elements are those which cannot contain
;; any other elements. Paragraphs are a lesser element -- they can
;; contain any number of objects, but cannot contain other elements
;; themselves. 'Greater' elements can contain other elements - lesser
;; or greater.

;; Finally we have two high-level classes -- Headings and
;; Sections. Sections contain both lesser and greater elements, and
;; headings contain an optional section and any number of child
;; headings.

;;; Code:
(defpackage :organ
  (:use :cl :cl-ppcre :std :parse/lex :sb-gray)
  (:import-from :uiop :read-file-string)
  (:export
   ;; vars
   :*org-todo-keyword-types*
   :*org-todo-keywords*
   :org-emphasis-alist
   :org-todo-keyword-map
   :org-headline-rx
   :org-file-property-rx
   :org-todo-keyword-rx
   :org-property-rx
   :org-priority-rx
   :org-property-start-rx
   :org-logbook-start-rx
   :org-end-rx
   :org-scheduled-rx
   :org-deadline-rx
   :org-src-block-rx
   :org-tag-rx
   :org-object-rx
   :org-timestamp-rx
   :org-ts-rx
   :org-table-any-line-rx
   :org-table-any-border-rx
   :org-tblfm-rx
   :org-footnote-definition-rx
   :*org-duration-hmm-rx*
   :*org-duration-hmmss-rx*
   :*org-duration-full-rx*
   :*org-duration-mixed-rx*
   :org-duration-units
   :org-list-full-item-rx
   :org-item-rx
   :org-element-types
   :org-element-objects
   ;; proto
   :org-parse
   :org-parse-lines
   :org-create
   :org-push
   :org-write
   :org-contents
   :org-property
   :org-get-element
   :org-insert-before
   ;; classes
   :org-element
   :text
   :org-document
   :doc-meta
   :doc-tree
   :org-zeroth-section
   :org-lines
   :o-lines
   :org-stream
   :org-headline
   :org-priority-level
   :props
   :tags
   :title
   :state
   :org-todo-keyword
   :todo-type
   :org-list
   :org-tag
   :org-paragraph
   :org-block
   :org-node-property
   :org-file-property
   :org-todo-keyword-p
   :org-tag-split
   ;; obj
   :org-heading
   :org-file-properties
   :org-node-properties
   :org-block
   :org-collection
   ;; util
   :read-org-string
   :read-org-file
   :read-org-lines
   :read-org-lines-from-string
   ;; TODO 2024-06-05: 
   :org-diary-sexp :org-footnote-reference :org-clock
   :org-bold :org-drawer :org-table-cell :org-citation
   :org-active-timestamp :org-descriptive-list
   :org-export-snippet :org-citation-reference :org-target
   :org-standard-table-row :org-lesser-block :org-strike-through
   :org-affiliated-keyword :org-planning-line
   :org-inline-source-block :org-footnote-definition
   :org-unordered-list :org-ordered-list :org-code
   :org-inactive-timestamp :org-keyword
   :org-inactive-timestamp-range :org-stat-cookie :org-macro
   :org-radio-target :org-table :org-table-el :org-italic
   :org-link :org-underline :org-entity :org-rule-table-row
   :org-verbatim :org-inline-babel-call :org-latex-environment
   :org-priority :org-property-drawer :org-plain-text
   :org-line-break :org-comment :org-greater-block
   :org-horizontal-rule :org-planning
   :org-active-timestamp-range))

(defpackage :organ/graph
  (:use :cl :std :organ :graph :db :rdb :seq :query :id :uuid :q :schema :graph :ast :time :b3)
  (:export :init-org-graph :org-graph :org-id 
   :org-graph-schema :*org-graph-schema* :*org-graph-db-directory* :init-org-graph-db 
   :org-graph-db :*org-graph-file* :*org-id-locations-file* :*org-graph*
   :org-graph-node :org-graph-edge :org-graph-extract-files :close-org-graph-db
   :open-org-graph-db :og-get :org-graph-values :org-graph-files-scrape))

(defpackage :organ/cli
  (:use :cl :std :organ :cli)
  (:export :*organ-cli*))
