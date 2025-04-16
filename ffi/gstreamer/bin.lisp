;;; bin.lisp --- Gstreamer FFI Bins

;; 

;;; Code:
(in-package :gstreamer)

(define-opaque gst-bin-private)

(define-alien-type gst-bin
    (struct gst-bin
      (element gst-element)
      (numchildren int)
      (children (* glist))
      (children-cookie (unsigned 32))
      (child-bus (* gst-bus))
      (messages (* glist))
      (polling boolean)
      (state-dirty boolean)
      (clock-dirty boolean)
      (provided-clock (* gst-clock))
      (clock-provider (* gst-element))
      (priv (* gst-bin-private))
      (%gst-reserved (array gpointer #.+gst-padding+))))

(define-opaque gst-bin-class)
