;;; gst.lisp --- High-level GStreamer API

;; 

;;; Code:
(in-package :dsp/gst)
(load-gstreamer)
(load-gst-play)
(glib:load-glib)

;;; Gst Pipe

;; implements the PIPE protocol for the GST-PIPE class which wraps a foreign
;; pointer to a GST-PIPELINE struct.
(defclass gst-pipe (pipe) 
  ((sap :accessor sap :initarg :sap :initform nil)))
