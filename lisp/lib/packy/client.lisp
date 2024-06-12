;;; packy/client.lisp --- Packy Client

;; Client-side API for Packy.

;;; Commentary:

;; The client is responsible for fetching packages from a registry,
;; organizing packages, and making them available at build-time.

;;; Code:
(in-package :packy/client)

(defun pk-index (&optional (name "packs"))
  (let ((url (obj/uri:merge-uris (concatenate 'string name ".json") *packy-url*)))
    (with-input-from-string (s (req:get url))
      (dat/json::json-read s nil))))
