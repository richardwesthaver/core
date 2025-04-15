;;; pipeline.lisp --- Gst Pipelines

;; 

;;; Code:
(in-package :gstreamer)

(define-alien-type gst-pipeline
    (struct gst-pipeline
      (bin gst-bin)
      (fixed-clock (* gst-clock))
      (stream-time gst-clock-time)
      (delay gst-clock-time)
      (priv (* t))
      (%gst-reserved (array gpointer #.+gst-padding+))))

(define-alien-routine gst-pipeline-get-type gtype)
(define-alien-routine gst-pipeline-new (* gst-element) (name c-string))
(define-alien-routine gst-pipeline-get-bus (* gst-bus) (pipeline (* gst-pipeline)))
