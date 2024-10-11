;;; mini-object.lisp --- GStreamer FFI MiniObjects

;; 

;;; Code:
(in-package :gstreamer)

(define-opaque gst-mini-object)

(define-alien-type gst-mini-object-copy-function (* t))
(define-alien-type gst-mini-object-dispose-function (* t))
(define-alien-type gst-mini-object-free-function (* t))

(define-alien-enum (gst-mini-object-flags int)
                   :lockable (ash 1 0)
                   :lock-readonly (ash 1 1)
                   :may-be-leaked (ash 1 2)
                   :last (ash 1 4))

(define-alien-type gst-mini-object-t
  (struct gst-mini-object
          (type gtype)
          (refcount int)
          (lockstate int)
          (flags unsigned-int)
          (copy gst-mini-object-copy-function)
          (dispose gst-mini-object-dispose-function)
          (free gst-mini-object-free-function)
          (priv-uint unsigned-int)
          (priv-pointer gpointer)))
