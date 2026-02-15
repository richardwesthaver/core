;;; alsa.lisp --- low-level bindings to ALSA

;;; Commentary:

;;; Code:
(defpackage :alsa
  (:use :cl :std :sb-alien)
  (:import-from :sb-unix :off-t)
  (:export :load-asound
           :snd-pcm-state
           :snd-pcm-format
           :snd-pcm-access
           :snd-pcm-stream
           :snd-pcm-open
           :snd-pcm-close
           :snd-strerror
           :snd-pcm-set-params
           :snd-pcm-recover
           :snd-pcm-writei
           :snd-pcm-dump
           :alsa-element-type
           :alsa-format-type))

(in-package :alsa)

(define-alien-loader asound "/usr/lib/")

(define-alien-enum (snd-pcm-class)
  :generic 0
  :multi 1
  :modem 2
  :digitizer 3)

(define-alien-enum (snd-pcm-stream)
  :playback 0
  :capture 1)

(define-alien-enum (snd-pcm-access)
  :mmap-interleaved 0
  :mmap-noninterleaved 1
  :mmap-complex 2
  :rw-interleaved 3
  :rw-noninterleaved 4)

;; incomplete list of formats
(define-alien-enum (snd-pcm-format)
  :unknown -1
  :s8 0
  :u8 1
  :s16-le 2
  :s16-be 3
  :u16-le 4
  :u16-be 5
  :s24-le 6
  :s24-be 7
  :u24-le 8
  :u24-be 9
  :s32-le 10
  :s32-be 11
  :u32-le 12
  :u32-be 13
  :float-le 14
  :float-be 15
  :float64-le 16
  :float64-be 17
  :iec958-subframe-le 18
  :iec958-subframe-be 19
  :mu-law 20
  :a-law 21
  :ima-adpcm 22
  :mpeg 23
  :gsm 24
  :special 31
  :s24-3le 32
  :s24-3be 33
  :u24-3le 34
  :u24-3be 35)

(define-alien-enum (snd-pcm-state)
  :open 0
  :setup 1
  :prepared 2
  :running 3
  :xrun 4
  :draining 5
  :paused 6
  :suspended 7
  :disconnected 8)

(defconstant %seek-set 0)
(defconstant %seek-cur 1)
(defconstant %seek-end 2)

(define-alien-type snd-pcm (* t))
(define-alien-type snd-output (* t))

(define-alien-type snd-pcm-mode int)

(defar snd-pcm-open int (pcm (* snd-pcm)) (name c-string) (ty snd-pcm-stream) (mode snd-pcm-mode))

(defar snd-pcm-close int (pcm snd-pcm))

(defar snd-strerror c-string (errnum int))

;; TODO

(defar snd-pcm-set-params int
  (pcm snd-pcm)
  (format snd-pcm-format)
  (access snd-pcm-access)
  (channels unsigned-int)
  (rate unsigned-int)
  (soft-resample int)
  (latency unsigned-int))

(defar snd-pcm-recover int
  (pcm snd-pcm)
  (err int)
  (silent int))

(define-alien-type snd-pcm-sframes long)
(define-alien-type snd-pcm-uframes unsigned-long)

(defar snd-pcm-writei snd-pcm-sframes
  (pcm snd-pcm)
  (buffer (* t))
  (size snd-pcm-uframes))

(defar snd-output-stdio-attach int
  (outputp (* snd-output))
  (file (* t))
  (close int))

(defar snd-pcm-dump int
  (pcm snd-pcm)
  (out snd-output))

(define-alien-variable stdout (* t))

;;; Utils
(defun alsa-element-type (type)
  (cond ((equalp type '(signed-byte 16)) :int16)
	((eql type 'single-float) :float)
        ((eql type 'double-float) :double)
        ((equalp type '(unsigned-byte 8)) :uint8)
        ((equalp type '(signed-byte 8)) :int8)
        ((equalp type '(unsigned-byte 16)) :uint16)
        ((equalp type '(unsigned-byte 32)) :uint32)
        ((equalp type '(signed-byte 32)) :int32)
        (t (error "Invalid base type ~A" type))))

(defun alsa-format-type (element-type)
  (cond ((eql element-type 'single-float) :float-le)
        ((eql element-type 'double-float) :float64-le)
        ((equalp element-type '(unsigned-byte 8)) :u8)
        ((equalp element-type '(signed-byte 8)) :8)
        ((equalp element-type '(unsigned-byte 16)) :u16-le)
        ((equalp element-type '(signed-byte 16)) :s16-le)
        ((equalp element-type '(unsigned-byte 32)) :u32-le)
        ((equalp element-type '(signed-byte 32)) :s32-le)
        (t (error "Invalid base type ~A" element-type))))
