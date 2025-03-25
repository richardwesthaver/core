;;; gst.lisp --- High-level GStreamer API

;; 

;;; Code:
(in-package :dsp/gst)
(glib:load-glib)
(load-gstreamer)
(load-gst-play)

;; (gst-version-string)

;;; Gst Pipe
#+nil
(with-alien ((argc int 1)
             (argv (* c-string) (cast (clone-strings '("gst-play-1.0" "--help")) (* c-string))))
  (gst-init (addr argc) (addr argv))
  (gst-is-initialized)
  (gst-deinit))

;; implements the PIPE protocol for the GST-PIPE class which wraps a foreign
;; pointer to a GST-PIPELINE struct.
(defclass gst-pipe (pipe)
  ((sap :accessor sap :initarg :sap :initform nil)))
