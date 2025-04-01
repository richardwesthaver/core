;;; pcm.lisp --- PCM audio support

;; 

;;; Code:
(in-package :aud)

;; pcm stream with static internal buffer
(defclass pcm-stream (static-stream)
  ((sap :reader sap :initform nil)
   (device :reader device :initarg :device)
   (pcm-format :reader pcm-format :initarg :pcm-format)
   (element-type :reader :element-type :initarg :element-type)
   (samplerate :reader samplerate :initarg :samplerate :initform 44100)
   (direction :reader direction :initarg :direction)
   (channels :reader channels :initarg :channels)))

;; (io/static:make-static-vector 1024)
;; (* (std/alien::foreign-type-size 'sb-alien:double-float) 1024 2)
