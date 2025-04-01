;;; cfg.lisp --- MPK Config

;; 

;;; Code:
(in-package :mpk)

(defconfig mpk-config ()
  ((path :initform nil :initarg :path :type (or pathname null))
   (logger :initform (default-logger-config) :initarg :logger :type (or null logger-config))
   (mpd :initarg :mpd :type mpd:mpd-config)
   (jack :initarg :jack)
   (pipewire :initarg :pipewire)
   (alsa :initarg :alsa)
   (gstreamer :initarg :gstreamer)
   (metro :initarg :metro)
   (picard :initarg :picard)
   (transmission :initarg :transmission)
   (ytdl :initarg :ytdl)))

(defun load-mpkrc ())
