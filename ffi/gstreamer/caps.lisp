;;; caps.lisp --- GStreamer FFI Caps

;; 

;;; Code:
(in-package :gstreamer)

(define-alien-enum (gst-caps-flags int)
                   :any (ash (gst-mini-object-flags :last) 0))

(define-alien-type gst-caps
  (struct gst-caps
          (mini-object gst-mini-object)))
