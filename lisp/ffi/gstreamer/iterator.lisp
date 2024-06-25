;;; iterator.lisp --- GStreamer FFI Iterators

;; 

;;; Code:
(in-package :gstreamer)

(eval-always
  (define-opaque gst-iterator))

(define-alien-type gst-iterator-t
  (struct gst-iterator
          (copy (* t))
          (next (* t))
          (resync (* t))
          (free (* t))
          (pushed (* gst-iterator))
          (type gtype)
          (lock (* gmutex))
          (cookie (unsigned 32))
          (master-cookie (* (unsigned 32)))
          (size unsigned-int)
          (%gst-reserved (array gpointer #.+gst-padding+))))
                         
