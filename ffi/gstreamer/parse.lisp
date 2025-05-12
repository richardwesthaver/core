;;; parse.lisp --- GST Parse

;; 

;;; Code:
(in-package :gstreamer)

(defar gst-parse-launch (* gst-element)
  (pipeline-description c-string)
  (error (* (* gerror))))
