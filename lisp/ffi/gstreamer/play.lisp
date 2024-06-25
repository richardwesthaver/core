;;; play.lisp --- Gstreamer FFI Play API

;; 

;;; Code:
(in-package :gstreamer)

(define-alien-loader gst-play t "/usr/lib/" "gstplay-1.0")

(eval-always
  (define-opaque gst-play)
  (define-opaque gst-play-class)
  (define-opaque gst-play-signal-adapter)
  (define-opaque gst-play-signal-adapter-class)
  (define-opaque gst-play-video-renderer)
  (define-opaque gst-play-video-renderer-interface))

(define-alien-type gst-play-video-renderer-interface-t
  (struct gst-play-video-renderer-interface
          (parent-iface gtype-interface)
          (gst-element (* t))))

(define-alien-routine gst-play-state-get-type gtype)

(define-alien-routine gst-play-error-quark gquark)
;; (gst-play-state-get-type)
(define-alien-routine gst-play-error-get-type gtype)

(define-alien-enum (gst-play-error int)
                   :failed 0)

(define-alien-routine gst-play-error-get-name c-string (error gst-play-error))

(define-alien-enum (gst-play-state int)
                   :stopped 0
                   :buffering 1
                   :paused 2
                   :playing 3)

(define-alien-routine gst-play-state-get-name c-string (state gst-play-state))

(define-alien-enum (gst-play-message int)
                   :uri-loaded 0
                   :position-updated 1
                   :duration-changed 2
                   :state-changed 3
                   :buffering 4
                   :end-of-stream 5
                   :error 6
                   :warning 7
                   :video-dimensions-changed 8
                   :media-info-updated 9
                   :volume-changed 10
                   :mute-changed 11
                   :seek-done 12)

(define-alien-routine gst-play-message-get-name c-string (message-type gst-play-message))

(define-alien-routine gst-play-get-type gtype)

(define-alien-routine gst-play-new (* gst-play) (video-renderer (* gst-play-video-renderer)))

