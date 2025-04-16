;;; clock.lisp --- Gstreamer FFI Clock

;; 

;;; Code:
(in-package :gstreamer)

(define-opaque gst-clock-private)

(define-alien-enum (gst-clock-time (unsigned 64))
  :none 18446744073709551615)

(define-alien-type gst-clock-time-diff (signed 64))

(define-alien-type gst-clock-id gpointer)

(define-alien-type gst-clock
  (struct gst-clock
          (object gst-object)
          (priv (* gst-clock-private))
          (gst-reserved (array gpointer #.+gst-padding+))))
