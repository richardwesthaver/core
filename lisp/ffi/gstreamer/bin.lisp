;;; bin.lisp --- Gstreamer FFI Bins

;; 

;;; Code:
(in-package :gstreamer)

(eval-always
  (define-opaque gst-bin-private)
  (define-opaque gst-bin))

(define-alien-type gst-bin-t
    (struct gst-bin
            (element gst-element-t)
            (numchildren int)
            (children (* glist))
            (children-cookie (unsigned 32))
            (child-bus (* gst-bus))
            (messages (* glist))
            (polling boolean)
            (state-dirty boolean)
            (clock-dirty boolean)
            (provided-clock (* gst-clock))
            (clock-provider (* gst-element-t))
            (priv (* gst-bin-private))
            (%gst-reserved (array gpointer #.+gst-padding+))))

(define-opaque gst-bin-class)
