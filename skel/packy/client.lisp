;;; packy/client.lisp --- Packy Client

;; Client-side API for Packy.

;;; Commentary:

;; The client is responsible for fetching packages from a registry,
;; organizing packages, and making them available to the user.

;;; Code:
(in-package :skel/packy)

(defstruct packy-client)

(defun pk-index (&optional (name "packs"))
  (let ((url (obj/uri:merge-uris (concatenate 'string name ".json") *packy-url*)))
    (with-input-from-string (s (req:get url))
      (dat/json::json-read s nil))))

(defun init-packy (&key reset columns (tree-sitter t))
  (ensure-directories-exist *packy-home*)
  (when (and reset *packy-db*) (db:shutdown-db *packy-db* :wait t))
  (unless *packy-db* (init-packy-db))
  (unless (db:db-open-p *packy-db*)
    (when (probe-file (name *packy-db*))
      (rdb:load-opts *packy-db* :backfill :full))
    (progn
      (db:open-db *packy-db*)
      (when columns
        (apply 'db:open-columns *packy-db* columns))))
  (when tree-sitter
    (tree-sitter:load-tree-sitter)
    (tree-sitter:load-tree-sitter-alien)
    (tree-sitter:load-tree-sitter-bash)))
