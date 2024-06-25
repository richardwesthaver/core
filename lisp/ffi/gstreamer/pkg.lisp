;;; pkg.lisp --- GStreamer FFI

;; 

;;; Code:
(defpackage :gstreamer
  (:nicknames :gst)
  (:use :cl :std :sb-alien)
  (:import-from :glib :gmainloop :gmaincontext
   :gerror :g-main-loop-new :goptiongroup :ginitially-unowned
   :gmutex :gpointer :glist :grec-mutex
   :gtype :gquark :gtype-interface :gcond
   :gthread :gdestroy-notify :ghook-list)
  (:export :gst-version-string :gst-version :gst-init
   :gst-init-check :gst-deinit :gst-is-initialized))

(in-package :gstreamer)

;; (load-glib)

(define-alien-loader gstreamer t "/usr/lib/" "gstreamer-1.0")
;; (load-gstreamer)

(define-alien-routine gst-version void (major (* unsigned)) (minor (* unsigned)) (micro (* unsigned)) (nano (* unsigned)))

(define-alien-routine gst-version-string c-string)

(define-alien-routine gst-segtrap-is-enabled boolean)
(define-alien-routine gst-segtrap-set-enabled void (enabled boolean))
(define-alien-routine gst-registry-fork-is-enabled boolean)
(define-alien-routine gst-registry-fork-set-enabled void (enabled boolean))

(define-alien-routine gst-update-registry boolean)

(define-alien-routine gst-get-main-executable-path c-string)

(define-alien-routine gst-init void (argc (* int)) (argv (array c-string)))
(define-alien-routine gst-init-check void
  (argc (* int)) (argv (array c-string))
  (error (* (* gerror))))

(define-alien-routine gst-is-initialized boolean)

(define-alien-routine gst-init-get-option-group (* goptiongroup))

(define-alien-routine gst-deinit void)

(defconstant +gst-padding+ 4)

;; (with-alien ((loop (* gmainloop)))
;;   (g-main-loop-new loop nil))

