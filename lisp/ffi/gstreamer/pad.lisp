;;; pad.lisp --- Gstreamer FFI Pads

;; 

;;; Code:
(in-package :gstreamer)

(eval-always
  (define-opaque gst-pad)
  (define-opaque gst-pad-template))

(define-alien-type gst-pad-private (* t))

(define-alien-enum (gst-pad-presence int)
                   :always 0
                   :sometimes 1
                   :request 2)

(define-alien-enum (gst-pad-template-flags int)
                   :last (ash (gst-object-flags :last) 4))

(define-alien-enum (gst-pad-direction int)
                   :unknown 0
                   :src 1
                   :sink 2)

(define-alien-enum (gst-pad-mode int)
                   :none 0
                   :push 1
                   :pull 2)

(define-alien-type gst-pad-template-t
  (struct gst-pad-template
          (object gst-object-t)
          (name-template c-string)
          (direction gst-pad-direction)
          (presence gst-pad-presence)
          (caps (* gst-caps))
          (abi (array gpointer #.+gst-padding+))))

(define-alien-type gst-pad-activate-function (* t))
(define-alien-type gst-pad-activate-mode-function (* t))
(define-alien-type gst-pad-link-function (* t))
(define-alien-type gst-pad-unlink-function (* t))
(define-alien-type gst-pad-chain-function (* t))
(define-alien-type gst-pad-chain-list-function (* t))
(define-alien-type gst-pad-event-function (* t))
(define-alien-type gst-pad-query-function (* t))
(define-alien-type gst-pad-iter-int-link-function (* t))

(define-alien-type gst-pad-t
  (struct gst-pad
          (object gst-object-t)
          (element-private gpointer)
          (padtemplate (* gst-pad-template))
          (direction gst-pad-direction)
          (stream-rec-lock grec-mutex)
          (task (* gst-task))
          (block-cond gcond)
          (probes ghook-list)
          (mode gst-pad-mode)
          (activatefun gst-pad-activate-function)
          (activatedata gpointer)
          (activatenotify gdestroy-notify)
          (activatemodefunc gst-pad-activate-mode-function)
          (activatemodenotify gdestroy-notify)
          (peer (* gst-pad))
          (linkfunc gst-pad-link-function)
          (linkdata gpointer)
          (linknotify gdestroy-notify)
          (unlinkfunc gst-pad-unlink-function)
          (unlinkdata gpointer)
          (unlinknotify gdestroy-notify)
          (chainfunc gst-pad-chain-function)
          (chaindata gpointer)
          (chainnotify gdestroy-notify)
          (chainlistfunc gst-pad-chain-list-function)
          (chainlistdata gpointer)
          (chainlistnotify gdestroy-notify)
          (getrangedata gpointer)
          (getrangenotify gdestroy-notify)
          (eventfunc gst-pad-event-function)
          (eventdata gpointer)
          (eventnotify gdestroy-notify)
          (offset (signed 64))
          (queryfunc gst-pad-query-function)
          (querydata gpointer)
          (querynotify gdestroy-notify)
          (iterintlinkfunc gst-pad-iter-int-link-function)
          (iterintlinkdata gpointer)
          (iterintlinknotify gdestroy-notify)
          (num-probes int)
          (num-blocked int)
          (priv (* gst-pad-private))
          (abi gpointer)))
