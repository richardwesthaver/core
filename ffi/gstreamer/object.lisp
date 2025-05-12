;;; object.lisp --- Gstreamer FFI Objects

;; 

;;; Code:
(in-package :gstreamer)

(define-alien-enum (gst-object-flags int)
                   :may-be-leaked (ash 1 0)
                   :constructed (ash 1 1)
                   :last (ash 1 4))

(define-alien-type gst-object
    (struct gst-object
      (object ginitially-unowned)
      (lock gmutex)
      (name c-string)
      ;; (* gst-object)
      (parent (* t))
      (flags (unsigned 32))
      (control-bindings (* glist))
      (control-rate (unsigned 64))
      (last-sync (unsigned 64))
      (%gst-reserved gpointer)))

(define-opaque gst-object-class)

(defar gst-object-ref gpointer (object gpointer))
(defar gst-object-unref void (object gpointer))
(defar gst-clear-object void (* (* gst-object)))
(defar gst-object-ref-sink gpointer (object gpointer))
(defar gst-object-replace boolean (old (* (* gst-object))) (new (* gst-object)))
(defar gst-object-get-path-string c-string (object (* gst-object)))
(defar gst-object-check-uniqueness boolean (list (* glist)) (name c-string))

