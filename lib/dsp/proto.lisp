;;; proto.lisp --- DSP and Media Protocols

;; 

;;; Code:
(in-package :dsp/core)

(defclass media-codec (id)
  ((name :initarg :name :accessor name)))

(defclass audio-codec (media-codec)
  ((sample-rate :initarg :sample-rate)
   (sample-format :initarg :sample-format)))

(defclass video-codec (media-codec)
  ((pix-format)
   (frame-rate)
   (color-range)
   (color-space)))
  
(defclass media-meta () 
  ((metadata :initarg :metadata)
   (mime-type :initarg :mime-type :type mime-type)))

(defclass av-meta (media-meta)
  ((duration :initarg :duration)
   (bitrate :initarg :bitrate)
   (codec :initarg :codec :type media-codec)))

(defclass media-file (media-meta)
  ((path :initarg :path :accessor path)))

(defclass audio-file (av-meta media-file) ())
(defclass video-file (av-meta media-file) ())
(defclass image-file (media-file) ())
