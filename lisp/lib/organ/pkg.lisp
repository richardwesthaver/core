;;; lib/organ/pkg.lisp --- Organ.

;; This package contains a parsing framework for Org Syntax. It
;; loosely follows the org-element.el conventions.

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
  (:use :cl :cl-ppcre :std)
  (:import-from :sb-gray :fundamental-stream)
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
   :org-duration-hmm-rx
   :org-duration-hmmss-rx
   :org-duration-full-rx
   :org-duration-mixed-rx
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
   :org-file
   :org-lines
   :o-lines
   :org-stream
   :org-headline
   :level
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
   :read-org-lines-from-string))


