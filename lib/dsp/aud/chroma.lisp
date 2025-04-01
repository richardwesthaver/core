;;; chroma.lisp --- High-level CHROMAPRINT API

;; High-level wrapper for CHROMAPRINT IDs

;;; Code:
(in-package :aud)
(load-chromaprint)

;; REVIEW 2025-03-30: audio-fingerprint class?
(defclass chromaprint (id:id) ())
