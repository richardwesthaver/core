;;; bus.lisp --- Gstreamer FFI Bus

;; 

;;; Code:
(in-package :gstreamer)

(define-opaque gst-bus-private)

(define-alien-type gst-bus
    (struct gst-bus
      (object gst-object)
      (priv (* gst-bus-private))
      (%gst-reserved (array gpointer #.+gst-padding+))))

(define-opaque gst-bus-class)

(defar gst-bus-timed-pop-filtered (* gst-message) (bus (* gst-bus)) (timeout gst-clock-time))
