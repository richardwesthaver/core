;;; element-factory.lisp --- Gstreamer FFI Element Factories

;; 

;;; Code:
(in-package :gstreamer)

(define-opaque gst-element-factory)
(define-alien-type gst-element-factory-t (struct gst-element-factory))

(defar gst-element-factory-get-type gtype)
(defar gst-element-factory-find (* gst-element-factory) (name c-string))
(defar gst-element-factory-make (* gst-element)
  (factory-name c-string)
  (name c-string))
(defar gst-element-factory-create (* gst-element)
  (factory (* gst-element-factory))
  (name c-string))
