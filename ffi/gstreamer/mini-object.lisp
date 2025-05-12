;;; mini-object.lisp --- GStreamer FFI MiniObjects

;; 

;;; Code:
(in-package :gstreamer)

(define-alien-type gst-mini-object-copy-function (* t))
(define-alien-type gst-mini-object-dispose-function (* t))
(define-alien-type gst-mini-object-free-function (* t))

(define-alien-type gst-mini-object
  (struct gst-mini-object
    (type gtype)
    (lockstate int)
    (flags unsigned-int)
    (copy gst-mini-object-copy-function)
    (dispose gst-mini-object-dispose-function)
    (free gst-mini-object-free-function)
    (priv-uint unsigned-int)
    (priv-pointer gpointer)))

(define-alien-enum (gst-mini-object-flags int)
                   :lockable (ash 1 0)
                   :lock-readonly (ash 1 1)
                   :may-be-leaked (ash 1 2)
                   :last (ash 1 4))

(defar gst-mini-object-init void
  (mini-object (* gst-mini-object))
  (flags unsigned-int)
  (type gtype)
  (copy-func gst-mini-object-copy-function)
  (dispose-func gst-mini-object-dispose-function)
  (free-func gst-mini-object-free-function))

(defar gst-mini-object-ref (* gst-mini-object)
  (mini-object (* gst-mini-object)))

(defar gst-mini-object-unref void
  (mini-object (* gst-mini-object)))
