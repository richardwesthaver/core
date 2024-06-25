;;; caps.lisp --- GStreamer FFI Caps

;; 

;;; Code:
(in-package :gstreamer)

(define-alien-enum (gst-caps-flags int)
                   :any (ash (gst-mini-object-flags :last) 0))

(eval-always
  (define-opaque gst-caps))

(define-alien-type gst-caps-t
  (struct gst-caps
          (mini-object gst-mini-object-t)))
