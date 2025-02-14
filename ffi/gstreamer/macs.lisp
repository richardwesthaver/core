;;; macs.lisp --- GStreamer Low-level Macros

;; 

;;; Code:
(in-package :gstreamer)

(defmacro with-gst-init ((sym &rest args) &body body)
  `(with-alien ((argv (array c-string ,(length args)) (clone-strings args))
                (argc (* int)))
     (let ((,sym (gst-init argc argv)))
