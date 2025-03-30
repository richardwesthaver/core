;;; pcm.lisp --- PCM audio support

;; 

;;; Code:
(in-package :aud)

(defclass pcm-stream (sb-gray:fundamental-stream)
  ((sap :reader sap :initform nil)
   (device :reader device :initarg :device)
   (pcm-format :reader pcm-format :initarg :pcm-format)
   (buffer :reader buffer :initarg :buffer)
   (buffer-size :reader buffer-size :initarg :buffer-size)
   (element-type :reader :element-type :initarg :element-type)
   (sample-rate :reader sample-rate :initarg :sample-rate :initform 44100)
   (direction :reader direction :initarg :direction)
   (channels :reader channels :initarg :channels)))
   
;; (io/static:make-static-vector 1024)
;; (* (std/alien::foreign-type-size 'sb-alien:double-float) 1024 2)
