;;; jpeg.lisp --- High-level JPEG Data Protocol

;; This package uses the DAT/IMG protocol to wrap the JPEG alien system which
;; provides bindings to libjpeg and libjpegturbo.

;;; Code:
(in-package :dat/jpeg)

(defclass jpeg-image (image) ())
