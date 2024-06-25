;;; debug.lisp --- Gstreamer FFI Debugging

;; 

;;; Code:
(in-package :gstreamer)

(define-alien-enum (gst-debug-graph-details int)
                   :show-media-type (ash 1 0)
                   :show-caps-details (ash 1 1)
                   :show-non-default-params (ash 1 2)
                   :show-states (ash 1 3)
                   :show-full-params (ash 1 4)
                   :show-all (- (ash 1 4) 1)
                   :show-verbose #xffffffff)

(define-alien-routine gst-debug-bin-to-dot-data c-string
  (bin (* gst-bin))
  (details gst-debug-graph-details))
