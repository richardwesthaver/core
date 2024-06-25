;;; object.lisp --- Gstreamer FFI Objects

;; 

;;; Code:
(in-package :gstreamer)

(eval-always
  (define-opaque gst-object))

(define-alien-type gst-object-t
  (struct gst-object
          (object ginitially-unowned)
          (lock gmutex)
          (name c-string)
          (parent (* gst-object))
          (flags (unsigned 32))
          (control-bindings (* glist))
          (control-rate (unsigned 64))
          (last-sync (unsigned 64))
          (%gst-reserved gpointer)))

(define-opaque gst-object-class)

