;;; pkg.lisp --- libmatroska FFI

;; 

;;; Commentary:

;;; Code:
(defpackage :matroska
  (:use :cl :std :sb-alien)
  (:export :load-matroska :load-ebml :track-type
   :track-type*))

(in-package :matroska)

(define-alien-loader ebml "/usr/lib/")
(define-alien-loader matroska "/usr/lib/")

(define-alien-enum (track-type unsigned-char)
  :video 1
  :audio 2
  :complex 3
  :logo #x10
  :subtitle #x11
  :buttons #x12
  :control #x20)

(define-alien-enum (open-mode int)
  :read 0
  :write 1
  :create 2
  :safe 3)

(define-alien-enum (matroska-error int)
  :null 0)

(define-alien-type matroska-stream (* t))
(define-alien-type matroska-id (* t))
(define-alien-type matroska-track (* t))
(define-alien-type matroska-file-mode c-string)
(define-alien-type matroska-error-callback (function (* t) matroska-error c-string))

(defar matroska-plug-log int
  (callback matroska-error-callback))

(defar matroska-unplug-log int
  (callback matroska-error-callback))

(defar matroska-open-stream-file matroska-stream
  (string c-string)
  (mode open-mode))

(defar matroska-open-stream matroska-id
  (stream matroska-stream))

(defar matroska-open-url matroska-id
  (string c-string))

(defar matroska-close void
  (id matroska-id))

(defar matroska-end void
  (id matroska-id)
  (totaltime (unsigned 32)))

(defar matroska-create-track matroska-track
  (id matroska-id)
  (type track-type))

(defar matroska-read-head void
  (id matroska-id))

(defar matroska-read-tracks void
  (id matroska-id))

(defar matroska-get-number-track unsigned-char
  (id matroska-id))

(defar matroska-get-track matroska-track
  (id matroska-id)
  (track-index unsigned-char))

(defar matroska-get-track-info void
  (id matroska-id)
  (track matroska-track)
  (infos (* (* t))))
