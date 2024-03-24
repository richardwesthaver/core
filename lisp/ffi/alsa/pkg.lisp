;;; alsa.lisp --- low-level bindings to ALSA

;;; Commentary:

;;; Code:
(defpackage :alsa
  (:use :cl :std :sb-alien)
  (:import-from :sb-unix :off-t)
  (:export ))

(in-package :alsa)

(define-alien-loader "asound" t "/usr/lib/")
;; (load-asound)

(defconstant %seek-set 0)
(defconstant %seek-cur 1)
(defconstant %seek-end 2)

(define-alien-type snd-pcm (* t))
(define-alien-type snd-output (* t))

(define-alien-type snd-pcm-stream int)
(define-alien-type snd-pcm-mode int)

(define-alien-routine snd-pcm-open int (pcm (* snd-pcm)) (name c-string) (ty snd-pcm-stream) (mode snd-pcm-mode))

(define-alien-routine snd-pcm-close int (pcm snd-pcm))

(define-alien-routine snd-strerror c-string (errnum int))

;; TODO
(define-alien-type snd-pcm-format int)

(define-alien-type snd-pcm-access int)

(define-alien-routine snd-pcm-set-params int
  (pcm snd-pcm)
  (format snd-pcm-format)
  (access snd-pcm-access)
  (channels unsigned-int)
  (rate unsigned-int)
  (soft-resample int)
  (latency unsigned-int))

(define-alien-routine snd-pcm-recover int
  (pcm snd-pcm)
  (err int)
  (silent int))

(define-alien-type snd-pcm-sframes long)
(define-alien-type snd-pcm-uframes unsigned-long)

(define-alien-routine snd-pcm-writei snd-pcm-sframes
  (pcm snd-pcm)
  (buffer (* t))
  (size snd-pcm-uframes))

(define-alien-routine snd-output-stdio-attach int
  (outputp (* snd-output))
  (file (* t))
  (close int))

(define-alien-routine snd-pcm-dump int
  (pcm snd-pcm)
  (out snd-output))

(define-alien-variable stdout (* t))


