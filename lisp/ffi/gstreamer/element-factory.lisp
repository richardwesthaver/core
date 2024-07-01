;;; element-factory.lisp --- Gstreamer FFI Element Factories

;; 

;;; Code:
(in-package :gstreamer)

(define-opaque gst-element-factory)
(define-alien-type gst-element-factory-t (struct nil))

(define-alien-routine gst-element-factory-get-type gtype)
(define-alien-routine gst-element-factory-find (* gst-element-factory) (name c-string))
(define-alien-routine gst-element-factory-make (* gst-element)
  (factory (* gst-element-factory))
  (name c-string))
