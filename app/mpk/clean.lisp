;;; clean.lisp --- Media Cleanup Utils

;; Deduplication, empty directory deletion, etc

;;; Commentary:

;; Media is first downloaded to the cache, then processed inplace, and only
;; then are files transferred to the media directory. Still, things can go
;; wrong, duplicates added, files shifted around, etc which merits the need
;; for a dedicated 'cleanup' component, specialized to our media workflow.

;;;; Generic Cleanup

;; - delete empty directories
;; - calc a b3sum and prompt to delete byte-for-byte duplicates
;; - compare matching filenames and prompt for deletion

;;;; Audio

;; - calculate an AcoustID and prompt to delete matches

;;; Code:
(in-package :mpk)
