;;; parse.lisp --- GST Parse

;; 

;;; Code:
(in-package :gstreamer)

(define-alien-routine gst-parse-launch (* gst-element)
  (pipeline-description c-string)
  (error (* (* gerror))))
