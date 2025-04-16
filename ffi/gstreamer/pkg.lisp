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
           :load-gstreamer :load-gst-play
   :gst-init-check :gst-deinit :gst-is-initialized :gst-element-factory-make
   :gst-element-set-state
   :gst-object-class
   :gst-object
   :gst-bus-private
   :gst-bus
   :gst-bus-class
   :gst-element
   :gst-element-class
   :gst-bin-private
   :gst-bin
   :gst-bin-class
   :gst-clock-private
   :gst-pad
   :gst-pad-template
   :gst-play
   :gst-play-class
   :gst-play-signal-adapter
   :gst-play-signal-adapter-class
   :gst-play-video-renderer
   :gst-play-video-renderer-interface
   :gst-caps
   :gst-mini-object
   :gst-task
   :gst-task-private
   :gst-task-class
   :gst-iterator
   :gst-element-factory
   :gst-element-factory-find
   :gst-element-factory-get-type
   :gst-element-factory-create
   :gst-object-unref
   :gst-object-ref
   :gst-object-get-path-string
   :gst-object-check-uniqueness
   :gst-object-replace
   :gst-object-ref-sink
   :gst-object-flags
   :gst-play-state
   :gst-play-state-get-name
   :gst-play-message
   :gst-play-message-get-name
   :gst-play-get-type
   :gst-play-new
   :gst-play-error
   :gst-state
   :gst-pipeline
   :gst-pipeline-new
   :gst-pipeline-get-bus
   :gst-pipeline-get-type
   :gst-parse-launch
   :gst-element-set-bus
   :gst-element-get-bus
   :gst-element-set-context
   :gst-element-get-context
   :gst-element-get-state
   :load-ges
   :ges-init
   :ges-init-check
   :ges-is-initialized
   :ges-deinit
   :gst-message-type
   :gst-message
   :gst-message-type-get-name
   :gst-message-get-type
   :gst-message-ref
   :gst-message-unref
   :gst-clock-time
   :gst-message-code
   :gst-task-state
   :gst-bus-timed-pop-filtered
   :gst-state-change-return
   :gst-state-change-return*
   :with-gst-init))

(in-package :gstreamer)

;; (load-glib)

(define-alien-loader gstreamer "/usr/lib/" "gstreamer-1.0")
(define-alien-loader ges "/usr/lib/" "ges-1.0")
;; (load-gstreamer)
;; (load-ges)

(define-alien-routine gst-version void (major (* unsigned)) (minor (* unsigned)) (micro (* unsigned)) (nano (* unsigned)))

(define-alien-routine gst-version-string c-string)

(define-alien-routine gst-segtrap-is-enabled boolean)
(define-alien-routine gst-segtrap-set-enabled void (enabled boolean))
(define-alien-routine gst-registry-fork-is-enabled boolean)
(define-alien-routine gst-registry-fork-set-enabled void (enabled boolean))

(define-alien-routine gst-update-registry boolean)

(define-alien-routine gst-get-main-executable-path c-string)

(define-alien-routine gst-init void (argc (* int)) (argv (* (* c-string))))
(define-alien-routine gst-init-check void
  (argc (* int)) (argv (* (* c-string)))
  (error (* (* gerror))))

(define-alien-routine gst-is-initialized boolean)

(define-alien-routine gst-init-get-option-group (* goptiongroup))

(define-alien-routine gst-deinit void)

(defconstant +gst-padding+ 4)

;; (with-alien ((loop (* gmainloop)))
;;   (g-main-loop-new loop nil))
