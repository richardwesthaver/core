;;; bus.lisp --- Gstreamer FFI Bus

;; 

;;; Code:
(in-package :gstreamer)

(eval-always
  (define-opaque gst-bus-private)
  (define-opaque gst-bus))

(define-alien-type gst-bus-t
  (struct gst-bus
          (object gst-object-t)
          (priv (* gst-bus-private))
          (%gst-reserved (array gpointer #.+gst-padding+))))

(define-opaque gst-bus-class)
